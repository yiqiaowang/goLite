module Spec.GoLite where


import Test.Hspec
import Test.Hspec.Core.Runner(hspecResult)
import Data.Either(isLeft, isRight)
import Control.Monad(forM_)


import GoLite


type Programs = [(String, String)]


spec :: Programs -> Programs -> Programs -> Spec
spec invalidParser invalidWeeder validSyntax =
  describe "GoLite" $ do
    forM_ invalidWeeder $ \(file, text) ->
      it ("correctly parses but fails in weeder phase : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isWeederError

    forM_ invalidParser $ \(file, text) ->
      it ("correctly parses but fails in weeder phase : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isParserError

    forM_ validSyntax $ \(file, text) ->
      it ("correctly parses and weeds : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isRight

    where
      isParserError (Left (ParserError _)) = True
      isParserError _ = False
      isWeederError (Left (WeederError _)) = True
      isWeederError _ = False
