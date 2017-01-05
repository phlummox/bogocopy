
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where

import Control.Monad.IO.Class (MonadIO(..))

import Data.IORef
import Data.List                      (sort, sortBy, isPrefixOf, stripPrefix)
import Data.Maybe                     (fromJust)
import Data.Monoid                    ( (<>) )
import Data.Ord                       (comparing, Down(..))
import Data.Text                      (Text(..))
import qualified Data.Text as T
import System.Directory               (copyFileWithMetadata)
import System.FilePath                (splitPath, joinPath)
import System.FilePath.Find           (find, always, fileType, (==?)
                                      , FileType(..), FindClause(..)
                                      , (&&?), (<=?) )
import qualified System.FilePath.Find as Fd (fileSize)
import System.IO                      (hSetBuffering, stdout, BufferMode(..))
import System.Posix.Files             (fileSize, modificationTime
                                      , getSymbolicLinkStatus
                                      , setFileTimesHiRes, accessTimeHiRes
                                      , modificationTimeHiRes, getFileStatus)
import System.Posix.Types             (COff(..))
import System.IO.Unsafe

import qualified Shelly.Lifted as S
import Shelly.Lifted hiding (find, FilePath)

import Trunc                          (truncate)
import CmdArgs                        (CmdArgs(..), withCmdArgs)

default (Text)

-- | naughty. shift these into a reader monad or something.
{-# NOINLINE debugRef #-}
debugRef = unsafePerformIO $ newIORef False

{-# NOINLINE debug #-}
debug = unsafePerformIO $ readIORef debugRef

-- | number of levels in a path
depth path = length $ splitPath path


-- | basically, makes srcFile relative to srcDir,
-- then substitutes in dstDir instead.
--
-- will throw exception on empty strings.
replaceDir :: String -> String -> String -> String
replaceDir srcDir dstDir srcFile=
  let
    srcDir' = if last srcDir /= '/' then srcDir <> "/" else srcDir
    dstDir' = if last dstDir /= '/' then dstDir <> "/" else dstDir
  in
    replaceDir_ srcDir' dstDir' srcFile
  where
    replaceDir_ srcDir dstDir srcFile =
      if srcDir `isPrefixOf` srcFile 
        then let fileBit = fromJust $ stripPrefix srcDir srcFile
             in  joinPath [dstDir, fileBit]
        else error $ "replaceDir: srcDir '" <> srcDir <> "' is not prefix of srcFile '" <> srcFile <> "'"

      
-- | replaceDir' - version for Text
replaceDir' srcDir dstDir srcFile =
  T.pack $ replaceDir (T.unpack srcDir) (T.unpack dstDir) (T.unpack srcFile)


-- NB:
-- "cp -a"
-- is same as System.Directory.copyFileWithMetadata

-- | eitherFiles pred f g src dst --
-- where src and dst are directory names,
-- will process all the files under src according
-- to pred. If they pass, it processes them with "f"
-- (takes the src file path and the dest file path),
-- otherwise with "g".
eitherFiles
  :: FindClause Bool
     -> (Text -> Text -> Sh a)
     -> (Text -> Text -> Sh a)
     -> Text
     -> Text
     -> Sh ([a], [a])
eitherFiles  = 
  filtSplitFiles (fileType ==? RegularFile) 


-- | same as eitherFiles, but return Sh ().
eitherFiles_
  :: FindClause Bool
     -> (Text -> Text -> Sh ())
     -> (Text -> Text -> Sh ())
     -> Text
     -> Text
     -> Sh ()
eitherFiles_ pred f g src dst = do
  _ <- eitherFiles pred f g src dst
  return ()

-- same as eitherFiles, but for directories.
eitherDirs    = 
  filtSplitFiles (fileType ==? Directory)   

-- | filtSplitFiles filterPred splitPred f g src dst:
--
-- iterate over the files in directory src.
-- filter for ones matching filterPred, discarding all others.
-- then split them into 2 - those matching splitPred, and those not.
-- On those that do, execute f, on the others, execute g.
-- Return the results
filtSplitFiles
  :: Control.Monad.IO.Class.MonadIO m =>
     FindClause Bool
     -> FindClause Bool
     -> (Text -> Text -> m a)
     -> (Text -> Text -> m b)
     -> Text
     -> Text
     -> m ([a], [b])
filtSplitFiles filterPred splitPred f g src dst = do
  predDirs <- liftIO $ find always (filterPred &&? splitPred) (T.unpack src)
  otherfiles <- liftIO $ find always (filterPred &&? (not <$> splitPred)) (T.unpack src) 
  let getNewName = replaceDir' src dst
      doF p =  f p (getNewName p)
      doG p =  g p (getNewName p)

  good <- mapM (doF . T.pack) predDirs
  ungood <- mapM (doG . T.pack) otherfiles
  return (good, ungood)


-- | bogoCopy pred srcDir dstDir: make a 'clone' of srcDir at dstDir;
-- but if a file passes "pred", then make a real copy,
-- but if not, just make a zero-size sparse file with the same
-- name and attributes.
--bogoCopy
--  :: System.FilePath.Find.FindClause Bool
bogoCopy :: FindClause Bool -> Text -> Text -> Sh ()
bogoCopy pred srcDir dstDir = do
  let isDir = fileType ==? Directory
  when debug $
    echo_err "copying directory structure" 
  tree_cp srcDir dstDir
  when debug $
    echo_err "copying files" 
  eitherFiles_ pred real_cp zero_cp srcDir dstDir
  let -- return the src, and an action to clone the time
      depthAndSetTime :: Text -> Text -> Sh (String, Sh ())
      depthAndSetTime src dst =
            return (T.unpack src, time_cp src dst)
  when debug $
    echo_err "cloning dir times" 
  -- set the times, running actions in reverse order of depth
  -- (i.e. deepest first) 
  (xs, _) <- eitherDirs isDir 
                         depthAndSetTime 
                         (\_ _ -> return ()) 
                         srcDir 
                         dstDir 
  sequence_ $ snd $ unzip $ sortBy (comparing (Down . fst)) xs


fromPath = T.unpack . toTextIgnore 

-- | FilePath version of fileSize
fileSize' path = 
  fileSize <$> (getFileStatus . fromPath) path
  

hasSize pred path = do
  sz <- fileSize' path
  return $ pred sz
  

real_cp :: Text -> Text -> Sh ()
real_cp src dst = do
  -- cmd "cp" "-d" "--preserve=all" src dst
  when debug $
    echo_err $ "real_cp " <> src <> " " <> dst
  liftIO $ copyFileWithMetadata (T.unpack src) (T.unpack dst)

zero_cp :: Text -> Text -> Sh ()
zero_cp src dst = do
  when debug $
    echo_err $ "zero_cp " <> src <> " " <> dst
  cmd "cp" "--attributes-only" "--preserve=all" src dst
  stat <- liftIO $ getSymbolicLinkStatus (T.unpack src)
  let aTime = accessTimeHiRes stat
      mTime = modificationTimeHiRes stat
      sz = fileSize stat 
  liftIO $ do
      Trunc.truncate (T.unpack dst) 0
      Trunc.truncate (T.unpack dst) (fromIntegral sz)
      setFileTimesHiRes (T.unpack dst) aTime mTime

time_cp :: Text -> Text -> Sh ()
time_cp src dst = do
  when debug $
    echo_err $ "time_cp " <> src <> " " <> dst
  -- cmd "cp" "--attributes-only" "--preserve=all" src dst
  stat <- liftIO $ getSymbolicLinkStatus (T.unpack src)
  let aTime = accessTimeHiRes stat
      mTime = modificationTimeHiRes stat
      sz = fileSize stat 
  liftIO $ 
      setFileTimesHiRes (T.unpack dst) aTime mTime


-- | make copy of directory tree. should preserve modification times, ownership/permissions
tree_cp :: Text -> Text -> Sh ()
tree_cp src dst = do
  when debug $
    echo_err $ "tree_cp " <> src <> " " <> dst
  run_ "rsync" ["-avAt", "--include", "*/", "--exclude", "*", src, dst]

-- | src and dst are source and dest dirs.
-- maxSize is size less than which, in MB, we should make real copies
mainSh
  :: String -> String -> COff -> IO ()
mainSh src dst maxSizeBytes = do
  hSetBuffering stdout LineBuffering
  let verbosify = if debug
                  then verbosely
                  else silently
  shelly $ verbosify $  
    bogoCopy (Fd.fileSize <=? maxSizeBytes) (T.pack (src <> "/")) (T.pack dst) 
main :: IO ()
main = withCmdArgs $ 
        \(CmdArgs.CmdArgs verbose bytesSize srcDir dstDir) -> do
          writeIORef debugRef verbose
          mainSh srcDir dstDir (fromIntegral bytesSize)






