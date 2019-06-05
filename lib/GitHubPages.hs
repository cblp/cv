module GitHubPages where

import Control.Monad    (unless)
import Data.List        (intercalate)
import Data.Monoid      ((<>))
import System.Directory (doesDirectoryExist, withCurrentDirectory)
import System.IO.Temp   (withSystemTempDirectory)
import System.Process   (callProcess, readProcess)

deploy
    :: (FilePath -> IO ())
       -- ^ Site build routine.
       -- Its argument is the target directory where the site must appear.
    -> IO ()
deploy build = do
    assertInRepoRoot
    assertBranchIsMaster
    originUrl <- git ["config", "remote.origin.url"]
    userEmail <- git ["config", "user.email"]
    headId <- git ["rev-parse", "HEAD"]
    withSystemTempDirectory "github-pages-deploy." $ \tmp -> do
        git_
            [ "clone"
            , "--config=user.email=" <> userEmail
            , "--no-checkout"
            , "."
            , tmp
            ]
        build tmp
        withCurrentDirectory tmp $ do
            git_ ["add", "--verbose", "."]
            git_ ["commit", "--quiet", "--reuse-message=" <> headId]
            git_ ["push", "--force", originUrl, "master:gh-pages"]
        git_ ["fetch"]
  where
    assertBranchIsMaster = do
        headValue <- readFile ".git/HEAD"
        unless (strip headValue == "ref: refs/heads/master") $
            error ("expected HEAD to be master, but got " ++ show headValue)
    assertInRepoRoot = do
        inRepoRoot <- doesDirectoryExist ".git"
        unless inRepoRoot $ error "must run from git repo root"
    git args = strip <$> readProcess "git" args ""
    git_ = callProcess "git"
    strip = intercalate "\n" . lines
