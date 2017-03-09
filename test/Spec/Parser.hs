module Spec.Parser where


import Control.Monad(forM_)
import Test.Hspec
import Test.Hspec.Core.Runner(hspecResult)
import Data.Either(isLeft, isRight)
import Control.Monad(forM_)
import System.FilePath(takeExtension)
import System.Directory(getDirectoryContents)


import qualified Parser.Parser as Parser


spec :: [(String, String)] -> [(String, String)] -> Spec
spec valid invalid =
  describe "Parser" $ do
    forM_ valid $ \(file, text) ->
      it ("correctly parses : " ++ file) $
        Parser.parse file text `shouldSatisfy` isRight

    forM_ invalid $ \(file, text) ->
      it ("fails to parse : " ++ file) $
        Parser.parse file text `shouldSatisfy` isLeft
