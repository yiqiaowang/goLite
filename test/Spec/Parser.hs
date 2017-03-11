module Spec.Parser where


import Control.Monad(forM_)
import Test.Hspec
import Test.Hspec.Core.Runner(hspecResult)
import Data.Either(isRight)


import GoLite


spec :: [(String, String)] -> [(String, String)] -> [(String, String)] -> Spec
spec invalidParser invalidWeeder validSyntax = do
  forM_ invalidParser $ \(file, text) ->
    it ("fails with a parser error : " ++ file) $
      GoLite.parse file text `shouldSatisfy` isParserError

  forM_ invalidWeeder $ \(file, text) ->
    it ("correctly parses but fails with a weeder error : " ++ file) $
      GoLite.parse file text `shouldSatisfy` isWeederError

  forM_ validSyntax $ \(file, text) ->
    it ("correctly parses and weeds : " ++ file) $
      GoLite.parse file text `shouldSatisfy` isRight

  where
    isParserError (Left (ParserError _)) = True
    isParserError _ = False
    isWeederError (Left (WeederError _)) = True
    isWeederError _ = False
