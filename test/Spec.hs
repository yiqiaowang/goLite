module Main where


import Test.Hspec
import Test.Hspec.Core.Runner(hspecResult, summaryFailures, Summary)
import System.FilePath(takeExtension)
import System.Directory(getDirectoryContents)
import System.Exit(exitSuccess, exitFailure)

import qualified Parser
import qualified Spec.Scanner
import qualified Spec.Parser
import qualified Spec.Pretty
import qualified Spec.GoLite


--
loadPrograms :: FilePath -> IO [(String, String)]
loadPrograms directory = do
  files <- getDirectoryContents directory
  let files' = filter (\file -> ".go" == takeExtension file) files in
    let minFiles = map (\file -> directory ++ "/" ++ file) files' in
      mapM (\file -> readFile file >>= \text -> return (file, text)) minFiles



-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = do
  validSyntax <- loadPrograms "programs/valid/syntax"
  invalidParserSyntax <- loadPrograms "programs/invalid/parser"
  invalidWeederSyntax <- loadPrograms "programs/invalid/weeder"

  scannerSummary <- hspecResult Spec.Scanner.spec
  parserSummary <- hspecResult $ Spec.Parser.spec validSyntax invalidParserSyntax
  weederSummary <- hspecResult $ Spec.GoLite.spec validSyntax invalidWeederSyntax
  -- prettySummary <- hspecResult $ Spec.Pretty.spec validSyntax

  if any (not . isSuccess) [scannerSummary, parserSummary]
    then exitFailure
    else exitSuccess

    where
      isSuccess :: Summary -> Bool
      isSuccess summary = summaryFailures summary == 0
