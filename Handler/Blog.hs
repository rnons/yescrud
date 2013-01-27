module Handler.Blog where

import Import
import Data.Time (getCurrentTime)

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

getBlogR :: Handler RepHtml
getBlogR = do
    muser <- maybeAuth
    (entryWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        $(widgetFile "create")

postBlogR :: Handler RepHtml
postBlogR = do
    muser <- maybeAuth
    ((result, entryWidget), enctype) <- runFormPost entryForm
    case result of
         FormSuccess entry -> do
             entryId <- runDB $ insert entry
             redirect $ EntryR entryId
         _ -> defaultLayout $ do
            $(widgetFile "create")
