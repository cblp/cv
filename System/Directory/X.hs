module System.Directory.X ( module System.Directory.X
                          , module System.Directory
                          ) where

import Control.Exception  ( bracket )
import System.Directory

-- | copied from directory-1.2.3.0
withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  bracket getCurrentDirectory setCurrentDirectory $ \ _ -> do
    setCurrentDirectory dir
    action
