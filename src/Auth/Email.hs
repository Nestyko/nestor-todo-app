{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Auth.Email where

import ClassyPrelude.Yesod
import Database.Persist
import Database.Persist.Sql
import Yesod.Auth
import Yesod.Form
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Model (User(..), Unique(UniqueUser), UserId)
import qualified Data.Text.Encoding as TE

data EmailLogin = EmailLogin
    { emailLoginEmail :: Text
    , emailLoginPassword :: Text
    }

authEmail :: (YesodAuth site, AuthId site ~ UserId, YesodPersist site, YesodPersistBackend site ~ SqlBackend, YesodPersistRunner site, RenderMessage site FormMessage, Yesod site) => AuthPlugin site
authEmail = AuthPlugin "email" dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["login"] = getLoginR >>= sendResponse
    dispatch _ _ = notFound

    loginWidget routeToParent = do
        let loginRoute = routeToParent $ PluginR "email" ["login"]
        (formWidget, _) <- liftHandler $ do
            emailLoginForm <- return $ renderBootstrap3 BootstrapBasicForm $ EmailLogin
                <$> areq emailField (FieldSettings { fsLabel = "Email", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control"), ("placeholder", "your@email.com")] }) Nothing
                <*> areq passwordField (FieldSettings { fsLabel = "Password", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control")] }) Nothing
            generateFormPost emailLoginForm
        toWidget [whamlet|
            <form method="post" action=@{loginRoute}>
                ^{formWidget}
                <button type="submit" .btn.btn-primary>Login
        |]

getLoginR :: (YesodAuth site, YesodPersist site, YesodPersistBackend site ~ SqlBackend, RenderMessage site FormMessage, Yesod site) => AuthHandler site Html
getLoginR = do
    emailLoginForm <- liftHandler $ return $ renderBootstrap3 BootstrapBasicForm $ EmailLogin
        <$> areq emailField (FieldSettings { fsLabel = "Email", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control"), ("placeholder", "your@email.com")] }) Nothing
        <*> areq passwordField (FieldSettings { fsLabel = "Password", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control")] }) Nothing
    (formWidget, formEnctype) <- liftHandler $ generateFormPost emailLoginForm
    loginRoute <- return $ routeToParent $ PluginR "email" ["login"]
    liftHandler $ defaultLayout $ do
        setTitle "Login"
        [whamlet|
            <div .row>
                <div .col-md-6.col-md-offset-3>
                    <h1>Login
                    <form method="post" action=@{loginRoute} enctype=#{formEnctype}>
                        ^{formWidget}
                        <div .form-group>
                            <button type="submit" .btn.btn-primary>Login
        |]
  where routeToParent = error "routeToParent not available in getLoginR"

postLoginR :: (YesodAuth site, AuthId site ~ UserId, YesodPersist site, YesodPersistBackend site ~ SqlBackend, RenderMessage site FormMessage) => AuthHandler site TypedContent
postLoginR = do
    ((result, _), _) <- liftHandler $ do
        emailLoginForm <- return $ renderBootstrap3 BootstrapBasicForm $ EmailLogin
            <$> areq emailField (FieldSettings { fsLabel = "Email", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control"), ("placeholder", "your@email.com")] }) Nothing
            <*> areq passwordField (FieldSettings { fsLabel = "Password", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control")] }) Nothing
        runFormPost emailLoginForm
    case result of
        FormSuccess form -> do
            let email = emailLoginEmail form
                password = emailLoginPassword form
            muser <- liftHandler $ runDB $ getBy $ UniqueUser email
            case muser of
                Just (Entity uid user) -> do
                    case userPassword user of
                        Just hashedPassword -> do
                            -- Simple password comparison (in production, use proper bcrypt)
                            -- For now, we'll use a simple check - in real app, use yesod-auth-email or proper bcrypt
                            let isValid = hashedPassword == password  -- TEMPORARY: Use proper hashing in production
                            if isValid
                                then setCredsRedirect $ Creds "email" email [("email", email)]
                                else do
                                    setMessage "Invalid email or password"
                                    redirect "/auth/page/email/login"
                        Nothing -> do
                            setMessage "Invalid email or password"
                            redirect "/auth/page/email/login"
                Nothing -> do
                    setMessage "Invalid email or password"
                    redirect "/auth/page/email/login"
        _ -> do
            setMessage "Please fix the errors below"
            redirect "/auth/page/email/login"
