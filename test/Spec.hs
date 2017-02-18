module Main where


import Control.Monad(forM_)
import System.FilePath(takeExtension)
import System.Directory(getDirectoryContents)
import Test.Hspec


import qualified Spec.Scanner


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
  hspec Spec.Scanner.spec

  -- validSyntax <- loadPrograms "programs/syntax/valid"
  -- invalidSyntax <- loadPrograms "programs/syntax/valid"
  --
  --
  -- hspec $ describe "Parser" $ do
  --   forM_ validSyntax $ \(file, text) ->
  --     it ("correctly parses : " ++ file) $
  --       pendingWith "need to write parser"
  --
  --   forM_ invalidSyntax $ \(file, text) ->
  --     it ("fails to parse : " ++ file) $
  --       pendingWith "need to write parser"
