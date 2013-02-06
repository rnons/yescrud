module Handler.Delete where

import Import

postDeleteR :: EntryId -> Handler RepHtml
postDeleteR  entryId = do
    runDB $ delete entryId
    redirect HomeR
