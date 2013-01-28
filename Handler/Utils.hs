module Handler.Utils where

import Data.Time (getCurrentTime)
import Import

getCurrentUser = do
    muser <- maybeAuth
    case muser of
         Just (Entity _ user) -> return $ userIdent user
         Nothing -> return ""

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" Nothing
    <*> aformM getCurrentUser
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" Nothing

updateForm :: Entry -> Form Entry
updateForm entry = renderDivs $ Entry
    <$> areq textField "Title" (Just $ entryTitle entry)
    <*> aformM getCurrentUser
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" (Just $ entryContent entry)
