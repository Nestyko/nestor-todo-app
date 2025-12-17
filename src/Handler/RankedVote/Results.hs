{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.Results where

import Import
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Ord (comparing)

-- Calculate Borda Count ranking
-- Takes item orders for each submission and calculates final ranking
calculateBordaRanking :: [Entity RankedVoteItem] -> [[(RankedVoteItemId, Int)]] -> [(Entity RankedVoteItem, Int)]
calculateBordaRanking items submissionOrders = 
    let totalItems = length items
        itemMap = Map.fromList $ map (\(Entity k v) -> (k, v)) items
        
        -- Calculate points for each item across all submissions
        itemPoints = foldl' accumulatePoints Map.empty submissionOrders
          where
            accumulatePoints acc itemOrderList = 
                foldl' (\acc' (itemId, position) -> 
                    let points = totalItems - position + 1
                    in Map.insertWith (+) itemId points acc'
                ) acc itemOrderList
        
        -- Convert to list and sort by points descending
        ranked = sortBy (flip $ comparing snd) $ Map.toList itemPoints
    in mapMaybe (\(itemId, points) -> 
        case Map.lookup itemId itemMap of
            Just item -> Just (Entity itemId item, points)
            Nothing -> Nothing
    ) ranked

getResultsR :: RankedVoteListId -> Handler Html
getResultsR listId = do
    (_, user) <- requireAuthPair
    userId <- return $ entityKey user
    
    mlist <- runDB $ get listId
    case mlist of
        Nothing -> notFound
        Just list -> do
            let isOwner = rankedVoteListOwner list == userId
            
            -- Get all items
            items <- runDB $ selectList [RankedVoteItemRankedVoteListId ==. listId] [Asc RankedVoteItemCreated]
            
            -- Get all submissions with their orders
            allSubmissions <- runDB $ selectList [RankedVoteSubmissionRankedVoteListId ==. listId] [Asc RankedVoteSubmissionSubmitted]
            
            -- Get orders for all submissions
            submissionOrders <- forM allSubmissions $ \(Entity subId _) -> do
                orders <- runDB $ selectList [RankedVoteItemOrderSubmissionId ==. subId] [Asc RankedVoteItemOrderPosition]
                return $ map (\(Entity _ order) -> (rankedVoteItemOrderItemId order, rankedVoteItemOrderPosition order)) orders
            
            -- Calculate Borda Count ranking
            let finalRanking = calculateBordaRanking items submissionOrders
            
            -- Get user's submission
            msubmission <- runDB $ getBy $ UniqueRankedVoteSubmission userId listId
            userOrderedItems <- case msubmission of
                Just (Entity subId _) -> do
                    orders <- runDB $ selectList [RankedVoteItemOrderSubmissionId ==. subId] [Asc RankedVoteItemOrderPosition]
                    return $ map (rankedVoteItemOrderItemId . entityVal) orders
                Nothing -> return []
            
            -- Create item lookup map
            let itemMap = Map.fromList $ map (\(Entity k v) -> (k, v)) items
            
            -- Get all individual submissions for owner
            individualSubmissions <- if isOwner
                then do
                    forM allSubmissions $ \(Entity subId submission) -> do
                        let subUserId = rankedVoteSubmissionUserId submission
                        msubUser <- runDB $ get subUserId
                        orders <- runDB $ selectList [RankedVoteItemOrderSubmissionId ==. subId] [Asc RankedVoteItemOrderPosition]
                        let orderedItemIds = map (rankedVoteItemOrderItemId . entityVal) orders
                        orderedItems <- forM orderedItemIds $ \itemId -> do
                            mitem <- runDB $ get itemId
                            return (itemId, mitem)
                        return (fmap (Entity subUserId) msubUser, orderedItems)
                else return []
            
            defaultLayout $ do
                setTitle "Ranked Vote Results"
                $(widgetFile "ranked-vote/results")

