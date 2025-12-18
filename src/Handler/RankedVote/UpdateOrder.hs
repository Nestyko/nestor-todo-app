{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.UpdateOrder where

import Import
import Data.Aeson (encode, object, (.=))
import qualified Data.Aeson as A

postUpdateOrderR :: Handler Value
postUpdateOrderR = do
    userId <- requireAuthId
    
    listIdText <- lookupPostParam "list_id"
    itemIds <- lookupPostParams "item_ids[]"
    
    case (listIdText, itemIds) of
        (Just listIdStr, _) | not (null itemIds) -> do
            case readMay listIdStr of
                Just listId -> do
                    mlist <- runDB $ get listId
                    case mlist of
                        Just list -> do
                            if rankedVoteListStatus list == "Closed"
                                then return $ object ["error" .= ("Poll is closed" :: Text)]
                                else do
                                    let itemIdTexts = mapMaybe (readMay . unpack) itemIds
                                    
                                    -- Get or create submission
                                    msubmission <- runDB $ getBy $ UniqueRankedVoteSubmission userId listId
                                    subId <- case msubmission of
                                        Just (Entity subId _) -> do
                                            -- Delete old orders
                                            runDB $ deleteWhere [RankedVoteItemOrderSubmissionId ==. subId]
                                            return subId
                                        Nothing -> do
                                            now <- liftIO getCurrentTime
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
                                    
                                    return $ object ["success" .= True]
                        Nothing -> return $ object ["error" .= ("Poll not found" :: Text)]
                Nothing -> return $ object ["error" .= ("Invalid poll ID" :: Text)]
        _ -> return $ object ["error" .= ("Missing parameters" :: Text)]




