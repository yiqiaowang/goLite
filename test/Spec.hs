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
  --load syntax programs
  validSyntax <- mconcat
    [ loadPrograms "programs/valid"
    , loadPrograms "programs/valid/syntax"
    ]
  invalidParser <- loadPrograms "programs/invalid/parser"
  invalidWeeder <- loadPrograms "programs/invalid/weeder"

  --load typechecker programs
  validTypeChecker <- mconcat
    [ loadPrograms "programs/valid/typechecker/declarations"
    , loadPrograms "programs/valid/typechecker/statements"
    , loadPrograms "programs/valid/typechecker/expressions"
    ]
  invalidTypeChecker <- loadPrograms "programs/invalid/typechecker"

  --execute tests
  scannerSummary <- hspecResult Spec.Scanner.spec
  prettySummary <- hspecResult $ Spec.Pretty.spec validSyntax
  goLiteSummary <- hspecResult $
    Spec.GoLite.spec
      invalidParser
      invalidWeeder
      validSyntax
      invalidTypeChecker
      validTypeChecker

  if any (not . isSuccess) [scannerSummary, prettySummary, goLiteSummary]
    then exitFailure
    else exitSuccess

    where
      isSuccess :: Summary -> Bool
      isSuccess summary = summaryFailures summary == 0
