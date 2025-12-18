{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.Participate where

import Import
import Data.Time (getCurrentTime)

getParticipateR :: RankedVoteListId -> Handler Html
getParticipateR listId = do
    userId <- requireAuthId
    
    mlist <- runDB $ get listId
    case mlist of
        Nothing -> notFound
        Just list -> do
            if rankedVoteListStatus list == "Closed"
                then do
                    setMessage "This poll is closed"
                    redirect $ RankedVoteR $ ResultsR listId
                else do
                    -- Get all items
                    items <- runDB $ selectList [RankedVoteItemRankedVoteListId ==. listId] [Asc RankedVoteItemCreated]
                    
                    -- Get user's existing submission
                    msubmission <- runDB $ getBy $ UniqueRankedVoteSubmission userId listId
                    orderedItems <- case msubmission of
                        Just (Entity subId _) -> do
                            orders <- runDB $ selectList [RankedVoteItemOrderSubmissionId ==. subId] [Asc RankedVoteItemOrderPosition]
                            return $ map (rankedVoteItemOrderItemId . entityVal) orders
                        Nothing -> return []
                    
                    defaultLayout $ do
                        setTitle "Participate in Ranked Vote"
                        $(widgetFile "ranked-vote/participate")

postParticipateR :: RankedVoteListId -> Handler Html
postParticipateR listId = do
    userId <- requireAuthId
    
    mlist <- runDB $ get listId
    case mlist of
        Nothing -> notFound
        Just list -> do
            if rankedVoteListStatus list == "Closed"
                then do
                    setMessage "This poll is closed"
                    redirect $ RankedVoteR $ ResultsR listId
                else do
                    -- Get item order from form
                    itemIds <- lookupPostParams "item_ids[]"
                    let itemIdTexts = mapMaybe (readMay . unpack) itemIds
                    
                    if null itemIdTexts
                        then do
                            setMessage "Please rank at least one item"
                            redirect $ RankedVoteR $ ParticipateR listId
                        else do
                            now <- liftIO getCurrentTime
                            
                            -- Get or create submission
                            msubmission <- runDB $ getBy $ UniqueRankedVoteSubmission userId listId
                            subId <- case msubmission of
                                Just (Entity subId _) -> do
                                    -- Delete old orders
                                    runDB $ deleteWhere [RankedVoteItemOrderSubmissionId ==. subId]
                                    return subId
                                Nothing -> do
                                    subId <- runDB $ insert RankedVoteSubmission
                                        { rankedVoteSubmissionUserId = userId
                                        , rankedVoteSubmissionRankedVoteListId = listId
                                        , rankedVoteSubmissionSubmitted = now
                                        }
                                    return subId
                            
                            -- Insert new orders
                            forM_ (zip itemIdTexts [1..]) $ \(itemId, position) -> do
                                _ <- runDB $ insert RankedVoteItemOrder
                                    { rankedVoteItemOrderSubmissionId = subId
                                    , rankedVoteItemOrderItemId = itemId
                                    , rankedVoteItemOrderPosition = position
                                    }
                                return ()
                            
                            setMessage "Your vote has been submitted!"
                            redirect $ RankedVoteR $ ViewR listId




