module Handler.Utils where

import Data.Time (getCurrentTime)
import Import

getCurrentUserId :: GHandler App App UserId
getCurrentUserId = do
    muser <- maybeAuthId
    case muser of
         Just uid -> return uid
         Nothing -> undefined

getCurrentUser :: GHandler App App Text
getCurrentUser = do
    muser <- maybeAuth
    case muser of
         Just (Entity _ user) -> return $ userIdent user
         Nothing -> return ""

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" Nothing
    -- <*> aformM maybeAuthId
    <*> aformM getCurrentUserId
    <*> aformM getCurrentUser
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" Nothing

updateForm :: Entry -> Form Entry
updateForm entry = renderDivs $ Entry
    <$> areq textField "Title" (Just $ entryTitle entry)
    -- <*> aformM maybeAuthId
    <*> aformM getCurrentUserId
    <*> aformM getCurrentUser
    <*> aformM (liftIO getCurrentTime)
    <*> areq textareaField "Content" (Just $ entryContent entry)
