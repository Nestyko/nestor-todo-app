{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.RankedVote.AddItem where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Time (getCurrentTime)

data AddItemForm = AddItemForm
    { addItemText :: Text
    }

addItemForm :: Form AddItemForm
addItemForm = renderBootstrap3 BootstrapBasicForm $ AddItemForm
    <$> areq textField (FieldSettings "Item" Nothing Nothing [("class", "form-control"), ("placeholder", "Enter item text")]) Nothing

postAddItemR :: RankedVoteListId -> Handler Html
postAddItemR listId = do
    (_, user) <- requireAuthPair
    userId <- return $ entityKey user
    
    mlist <- runDB $ get listId
    case mlist of
        Nothing -> notFound
        Just list -> do
            if rankedVoteListStatus list == "Closed"
                then do
                    setMessage "Cannot add items to a closed poll"
                    redirect $ RankedVoteR $ ViewR listId
                else do
                    ((result, _), _) <- runFormPost addItemForm
                    case result of
                        FormSuccess form -> do
                            now <- liftIO getCurrentTime
                            _ <- runDB $ insert RankedVoteItem
                                { rankedVoteItemRankedVoteListId = listId
                                , rankedVoteItemText = addItemText form
                                , rankedVoteItemCreatedBy = userId
                                , rankedVoteItemCreated = now
                                }
                            setMessage "Item added successfully!"
                            redirect $ RankedVoteR $ ViewR listId
                        _ -> do
                            setMessage "Please enter item text"
                            redirect $ RankedVoteR $ ViewR listId

