{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.Create where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Time (getCurrentTime)

data RankedVoteForm = RankedVoteForm
    { rankedVoteTitle :: Text
    , rankedVoteDescription :: Maybe Text
    }

rankedVoteForm :: Form RankedVoteForm
rankedVoteForm = renderBootstrap3 BootstrapBasicForm $ 
    RankedVoteForm
        <$> areq textField (FieldSettings { fsLabel = "Title", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control"), ("placeholder", "Enter vote title")] }) Nothing
        <*> aopt textField (FieldSettings { fsLabel = "Description", fsTooltip = Nothing, fsId = Nothing, fsName = Nothing, fsAttrs = [("class", "form-control"), ("placeholder", "Optional description"), ("rows", "3")] }) Nothing

getCreateR :: Handler Html
getCreateR = do
    requireAuth
    (formWidget, formEnctype) <- generateFormPost rankedVoteForm
    defaultLayout $ do
        setTitle "Create Ranked Vote"
        $(widgetFile "ranked-vote/create")

postCreateR :: Handler Html
postCreateR = do
    userId <- requireAuthId
    ((result, formWidget), formEnctype) <- runFormPost rankedVoteForm
    case result of
        FormSuccess form -> do
            now <- liftIO getCurrentTime
            listId <- runDB $ insert RankedVoteList
                { rankedVoteListOwner = userId
                , rankedVoteListTitle = rankedVoteTitle form
                , rankedVoteListDescription = rankedVoteDescription form
                , rankedVoteListStatus = "Open"
                , rankedVoteListCreated = now
                }
            setMessage "Ranked vote created successfully!"
            redirect $ RankedVoteR $ ViewR listId
        _ -> do
            setMessage "Please fix the errors below"
            defaultLayout $ do
                setTitle "Create Ranked Vote"
                $(widgetFile "ranked-vote/create")




