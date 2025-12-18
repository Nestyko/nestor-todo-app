{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.List where

import Import

getListR :: Handler Html
getListR = do
    userId <- requireAuthId
    
    -- Get lists created by user
    myLists <- runDB $ selectList [RankedVoteListOwner ==. userId] [Desc RankedVoteListCreated]
    
    -- Get lists user has participated in
    mySubmissions <- runDB $ selectList [RankedVoteSubmissionUserId ==. userId] []
    let participatedListIds = map (rankedVoteSubmissionRankedVoteListId . entityVal) mySubmissions
    participatedLists <- if null participatedListIds
        then return []
        else runDB $ selectList [RankedVoteListId <-. participatedListIds] [Desc RankedVoteListCreated]
    
    defaultLayout $ do
        setTitle "Ranked Votes"
        $(widgetFile "ranked-vote/list")

