{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Register where

import Import
import qualified Data.Text.Encoding as TE
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data RegisterForm = RegisterForm
    { registerEmail :: Text
    , registerPassword :: Text
    , registerPasswordConfirm :: Text
    }

registerForm :: Form RegisterForm
registerForm = renderBootstrap3 BootstrapBasicForm $ RegisterForm
    <$> areq emailField (FieldSettings "Email" Nothing Nothing [("class", "form-control"), ("placeholder", "your@email.com")]) Nothing
    <*> areq passwordField (FieldSettings "Password" Nothing Nothing [("class", "form-control")]) Nothing
    <*> areq passwordField (FieldSettings "Confirm Password" Nothing Nothing [("class", "form-control")]) Nothing

getRegisterR :: Handler Html
getRegisterR = do
    muid <- maybeAuthId
    when (isJust muid) $ redirect HomeR
    (formWidget, formEnctype) <- generateFormPost registerForm
    defaultLayout $ do
        setTitle "Register"
        $(widgetFile "register")

postRegisterR :: Handler Html
postRegisterR = do
    muid <- maybeAuthId
    when (isJust muid) $ redirect HomeR
    ((result, formWidget), formEnctype) <- runFormPost registerForm
    case result of
        FormSuccess form -> do
            let email = registerEmail form
                password = registerPassword form
                passwordConfirm = registerPasswordConfirm form
            
            if password /= passwordConfirm
                then do
                    setMessage "Passwords do not match"
                    defaultLayout $ do
                        setTitle "Register"
                        $(widgetFile "register")
                else do
                    existingUser <- runDB $ getBy $ UniqueUser email
                    case existingUser of
                        Just _ -> do
                            setMessage "Email already registered"
                            defaultLayout $ do
                                setTitle "Register"
                                $(widgetFile "register")
                        Nothing -> do
                            -- TEMPORARY: Store password as plain text for now
                            -- In production, use proper bcrypt hashing
                            -- For yesod-auth 1.6, consider using yesod-auth-email package
                            userId <- runDB $ insert User
                                { userIdent = email
                                , userPassword = Just password  -- TEMPORARY: Use proper hashing
                                }
                            setMessage "Registration successful! Please login."
                            redirect $ AuthR $ PluginR "email" ["login"]
        _ -> do
            setMessage "Please fix the errors below"
            defaultLayout $ do
                setTitle "Register"
                $(widgetFile "register")

