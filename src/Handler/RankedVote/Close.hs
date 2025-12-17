{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.Close where

import Import

postCloseR :: RankedVoteListId -> Handler Html
postCloseR listId = do
    (_, user) <- requireAuthPair
    userId <- return $ entityKey user
    
    mlist <- runDB $ get listId
    case mlist of
        Nothing -> notFound
        Just list -> do
            if rankedVoteListOwner list /= userId
                then do
                    setMessage "Only the poll owner can close it"
                    redirect $ RankedVoteR $ ViewR listId
                else if rankedVoteListStatus list == "Closed"
                    then do
                        setMessage "Poll is already closed"
                        redirect $ RankedVoteR $ ResultsR listId
                    else do
                        runDB $ update listId [RankedVoteListStatus =. "Closed"]
                        setMessage "Poll closed! Results are now available."
                        redirect $ RankedVoteR $ ResultsR listId

