module Handler.Update where

import Import
import Handler.Utils


getUpdateR :: EntryId -> Handler RepHtml
getUpdateR entryId = do
    entry <- runDB $ get404 entryId
    muser <- maybeAuth
    (entryWidget, enctype) <- generateFormPost $ updateForm entry
    defaultLayout $ do
        $(widgetFile "update")
        
postUpdateR :: EntryId -> Handler RepHtml
postUpdateR entryId = do
    muser <- maybeAuth
    ((result, entryWidget), enctype) <- runFormPost entryForm
    case result of
         FormSuccess entry -> do
             runDB $ replace entryId entry
             redirect $ EntryR entryId
         _ -> defaultLayout $ do
            $(widgetFile "create")
