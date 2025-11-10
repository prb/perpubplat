-- | Assemblage of functions for loading and saving an 'Item' from/to
-- a file.  The storage system works on the assumption that a post is
-- stored in a directory named for its permatitle and that comments
-- are then stored in subdirectories.
module Blog.BackEnd.IoOperations where

import qualified Blog.Model.Entry as B
import qualified Blog.Model.EntryParser as EP
import qualified Blog.Constants as C
import Utilities ( readFile', elapsed_hundreths, rights, lefts )

import qualified System.IO as I
import qualified System.IO.Error as SIE
import qualified System.FilePath as F
import qualified System.Directory as D

import System.FilePath ((</>), splitPath)
import Control.Monad (filterM)
import qualified Control.Exception as CE

import qualified Data.Time.Clock as DTC
import System.Log.Logger

log_handle :: String
log_handle = "IoOperations"

data LoadError = LoadError { content_path :: FilePath
                           , reason :: String }

instance Show LoadError where
    show le = "Unable to load content from " ++ (content_path le)
              ++ ": " ++ (reason le)

-- | Load all posts from disk.
boot :: IO B.Model
boot = do { t1 <- DTC.getCurrentTime
          ; all <- find_all is_content_file is_subdirectory C.content_storage_dir
          ; content <- mapM readFile' all
          ; t2 <- DTC.getCurrentTime
          ; infoM log_handle $ "Loaded " ++ (show $ length content) ++ " items (posts and comments) from "
                  ++ C.content_storage_dir ++ " in " ++ (elapsed_hundreths t2 t1) ++ " seconds."
          ; let loaded =  map (uncurry EP.item_from_string) $ zip all content
          ; let ok_items = rights loaded
          ; log_load_errors $ map (uncurry LoadError) $ lefts loaded 
          ; return $ B.build_model ok_items }

log_load_error :: LoadError -> IO ()
log_load_error = log_load_error_ log_handle

log_load_error_ :: String -> LoadError -> IO ()
log_load_error_ lh = (errorM lh) . show

log_load_errors :: [LoadError] -> IO ()
log_load_errors = log_load_errors_ log_handle

log_load_errors_ :: String -> [LoadError] -> IO ()
log_load_errors_ lh = mapM_ (log_load_error_ lh)

find_all :: (FilePath -> FilePath -> IO Bool)
         -> (FilePath -> FilePath -> IO Bool)
         -> FilePath -> IO [ FilePath ]
find_all file_test dir_test root = 
    do { dir_contents <- D.getDirectoryContents root
       ; debugM log_handle $ "Searching " ++ root ++ " for content."
       ; subdirs <- filterM (dir_test root) dir_contents
       ; debugM log_handle $ "Found " ++ (show $ length subdirs) ++ " subdirectories of "
                ++ root ++ " to search."
       ; files <- filterM (file_test root) dir_contents
       ; debugM log_handle $ "Found " ++ (show $ length files) ++ " files in " ++ root ++ "."
       ; others <- mapM (find_all file_test dir_test) (absolutize root subdirs)
       ; return $ (absolutize root files) ++ (concat others) }

absolutize :: FilePath -> [FilePath] -> [FilePath]
absolutize d = map (d </>)

is_content_file :: FilePath -> FilePath -> IO Bool
is_content_file d f = do { is_file <- D.doesFileExist (d </> f)
                       ; let is_content = ((last $ splitPath f) == "content.ppp")
                       ; return $ is_file && is_content }

is_subdirectory :: FilePath -> FilePath -> IO Bool
is_subdirectory d sd = do { is_dir <- D.doesDirectoryExist (d </> sd)
                       ; let is_not_dots =  not (sd `elem` [".",".."])
                       ; return $ is_dir && is_not_dots }

save :: B.Model -> B.Item -> IO Bool
save m i = do { D.createDirectoryIfMissing True d
              ; writeFile f (B.to_string i)
              ; return True}
           `CE.catch`
           \e -> do { errorM log_handle $ "Unable to store item in path "
                            ++ f ++ "; exception was: " ++ ( show (e :: CE.SomeException) )
                   ; return False }
    where
      d = C.content_storage_dir </> (B.ancestor_path m i)
      f = d </> "content.ppp"

dump :: B.Model -> IO ()
dump m = mapM_ (save m) (B.all_items m)

load_content :: FilePath -> IO (Either LoadError B.Item)
load_content = load_with log_handle EP.item_from_string (\g -> C.content_storage_dir </> g </> "content.ppp")

load_with :: String -> (String -> String -> Either (String, String) B.Item) -> (String -> String) -> FilePath -> IO (Either LoadError B.Item)
load_with lh parser base f = do { file <- (CE.try $ readFile' $ base f :: IO (Either CE.SomeException String))
                                ; case file of
                                    Right content ->
                                        do { let d = parser f $! content
                                           ; case d of
                                               Right i ->
                                                   return $ Right i
                                               Left (p,e) ->
                                                   do { let err = LoadError p e
                                                      ; log_load_error_ lh err
                                                      ; return $ Left err }
                                           }
                                    Left (ex :: CE.SomeException) ->

                                        do { let err = LoadError (base f) (show ex)
                                           ; log_load_error_ lh err
                                           ; return $ Left err }
                                }

load_draft :: FilePath -> IO (Either LoadError B.Item)
load_draft = load_with log_handle EP.draft_from_string (\f -> C.draft_dir </> f)

load_comment :: FilePath -> IO (Either LoadError B.Item)
load_comment = load_with log_handle EP.item_from_string (\f -> C.comment_dir </> f)

load :: String -> FilePath -> IO (Either LoadError B.Item) 
load lh = load_with lh EP.item_from_string id

update_item :: B.Model -> B.Item -> IO B.Model
update_item m i = do { content <- load_content $ B.ancestor_path m i
                     ; case content of
                         Right content' ->
                              do {m' <- B.alter (const content') m i
                                 ; b <- save m' (B.item_by_id m' $ B.internal_id i)
                                 ; if b then
                                       return m'
                                   else
                                       do { errorM log_handle $ "Unable to update model due to error saving updated content; model is unchanged."
                                          ; return m }
                                 }
                         Left _ ->
                             do { errorM log_handle "Unable to update model due to error loading updated content; model is unchanged."
                                ; return m }
                     }
