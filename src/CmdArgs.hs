

module CmdArgs where

import Data.Version                      (showVersion)
import qualified Paths_bogocopy          (version)


import Data.Monoid
import Options.Applicative hiding (helper)
import System.Environment


-- | A hidden \"helper\" option which always fails.
helper :: Parser (a -> a)
helper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short 'h'
  , help "Show this help text"
  , hidden ]

data CmdArgs = CmdArgs 
  {
      verbose :: Bool
    , maxSizeBytes :: Int
    , srcDir :: String
    , dstDir :: String
  }
  deriving (Eq, Show)

-- | int option reader
int :: ReadM Int
int = auto

intOption = option int


version = showVersion $ Paths_bogocopy.version

-- | parser for command-line args.
parseCmdArgs :: Parser CmdArgs
parseCmdArgs = CmdArgs 
  <$> switch
       ( short 'v' 
         <> long "verbose"
         <> help "verbose (debugging) output" )
  <*> (mBytesToBytes <$> intOption
         ( long "size"
         <> short 's'
         <> metavar "SIZE_MB"
         <> help "size limit, files leq to this size (in MB) are real-copied, those above are not" ))
  <*> argument str (metavar "SRCDIR")
  <*> argument str (metavar "DSTDIR"
                     <> help "will be created") 

  where
    mBytesToBytes x = 1024 * 1024 * x



testMain :: IO ()
testMain =
  withArgs [
              "-v"
             , "-s" , "100"
             , "/foo/bar/baz"
             , "/thing/dest"
           ] 
           main

-- | test this module out
main :: IO ()
main =  
  withCmdArgs print 

-- | parse command-line args, and execute an action
-- which takes them as an argument.
withCmdArgs :: (CmdArgs -> IO b) -> IO b
withCmdArgs f = execParser opts >>= f
  where
    opts = info (helper <*> parseCmdArgs)
      ( fullDesc
        <> progDesc desc
        <> header h )

    desc = "copy a directory tree, preserving permissions and modification times, but making zero-size sparse copies of big files"
    h    = "bogocopy v " <> version <> 
            " like rsync, but bogofy files over size limit" 



