module Handler.Update where

import Control.Monad.Reader
import Git
import Git.Utils
import Git.Libgit2
import Filesystem.Path.CurrentOS 

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
             repo <- liftIO $ openLgRepository (entryRepoPath entryId)
             liftIO $ runReaderT (runLgRepository action) repo
             redirect $ EntryR entryId
           where 
             action = do
                 let masterRef = "refs/heads/master"
                 cParent <- resolveRef masterRef
                 blob <- createBlobUtf8 (unTextarea $ entryContent entry)
                 tr <- newTree
                 putBlob tr (fromText $ entryTitle entry) blob
                 sig <- getCurrentUserSig $ entryAuthor entry
                 c <- createCommit [cParent] (treeRef tr) sig sig
                                  "Updated" (Just masterRef)
                 updateRef_ "refs/heads/master" (RefObj (commitRef c))
         _ -> defaultLayout $ do
            $(widgetFile "create")
