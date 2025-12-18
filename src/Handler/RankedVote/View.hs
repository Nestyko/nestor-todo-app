{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.View where

import Import

getViewR :: RankedVoteListId -> Handler Html
getViewR listId = do
    userId <- requireAuthId
    
    mlist <- runDB $ get listId
    case mlist of
        Nothing -> notFound
        Just list -> do
            let isOwner = rankedVoteListOwner list == userId
            let isOpen = rankedVoteListStatus list == "Open"
            
            -- Get all items
            items <- runDB $ selectList [RankedVoteItemRankedVoteListId ==. listId] [Asc RankedVoteItemCreated]
            
            -- Get user's submission if exists
            msubmission <- runDB $ getBy $ UniqueRankedVoteSubmission userId listId
            
            -- Get all submissions if owner
            allSubmissions <- if isOwner
                then runDB $ selectList [RankedVoteSubmissionRankedVoteListId ==. listId] [Asc RankedVoteSubmissionSubmitted]
                else return []
            
            defaultLayout $ do
                setTitle $ toHtml $ rankedVoteListTitle list
                $(widgetFile "ranked-vote/view")




