module Handler.Blog where

import Import
import Yesod.Auth
import Data.Time (getCurrentTime)

entryForm :: Form Entry
entryForm = renderDivs $ Entry
    <$> areq textField "Title" Nothing
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
