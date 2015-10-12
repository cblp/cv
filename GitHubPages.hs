module GitHubPages where

import Control.Monad
import Data.List
import Data.Monoid
import System.Directory.X ( doesDirectoryExist, withCurrentDirectory )
import System.Process
import System.IO.Temp     ( withSystemTempDirectory )

deploy :: (FilePath -> IO ()) -> IO ()
deploy build = do
    inRepoRoot <- doesDirectoryExist ".git"
    unless inRepoRoot $ error "must run from git repo root"
    headValue <- readFile ".git/HEAD"
    unless (strip headValue == "ref: refs/heads/master") $
        error ("expected HEAD to be master, but got " ++ show headValue)
    originUrl <- git ["config", "remote.origin.url"]
    userEmail <- git ["config", "user.email"]
    headId <- git ["rev-parse", "HEAD"]
    withSystemTempDirectory "github-pages-deploy." $ \tmp -> do
        git_  [ "clone", ".", tmp
              , "--config=user.email=" <> userEmail
              , "--no-checkout"
              ]
        build tmp
        withCurrentDirectory tmp $ do
            git_ ["add", "--verbose", "."]
            git_ ["commit", "--quiet", "--reuse-message=" <> headId]
            git_ ["push", "--force", originUrl, "master:gh-pages"]
        git_ ["fetch"]
  where
    strip = intercalate "\n" . lines
    git args = strip <$> readProcess "git" args ""
    git_ = callProcess "git"
