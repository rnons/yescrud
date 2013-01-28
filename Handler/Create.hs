module Handler.Create where

import Import
import Handler.Utils

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
