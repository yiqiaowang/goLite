module Spec.GoLite where


import Test.Hspec
import Test.Hspec.Core.Runner(hspecResult)
import Data.Either(isLeft, isRight)
import Control.Monad(forM_)


import GoLite


type Programs = [(String, String)]


spec
  :: Programs
  -> Programs
  -> Programs
  -> Programs
  -> Programs
  -> Spec
spec invalidParser invalidWeeder validSyntax invalidTypeChecker validTypeChecker =
  describe "GoLite" $ do
    forM_ invalidParser $ \(file, text) ->
      it ("fails with a parser error : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isParserError

    forM_ invalidWeeder $ \(file, text) ->
      it ("correctly parses but fails with a weeder error : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isWeederError

    forM_ validSyntax $ \(file, text) ->
      it ("correctly parses and weeds : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isRight

    forM_ invalidTypeChecker $ \(file, text) ->
      it ("correctly parses but fails to type check : " ++ file) $
        case GoLite.parse file text of
          Right program -> GoLite.typeCheck program `shouldSatisfy` isTypeError
          Left parseError -> fail $ show parseError

    forM_ validTypeChecker $ \(file, text) ->
      it ("correctly parses and type checks : " ++ file) $
        case GoLite.parse file text of
          Right program -> GoLite.typeCheck program `shouldSatisfy` isRight
          Left parseError -> fail $ show parseError

    where
      isParserError (Left (ParserError _)) = True
      isParserError _ = False
      isWeederError (Left (WeederError _)) = True
      isWeederError _ = False
      isTypeError (Left (TypeCheckerError _)) = True
      isTypeError _ = False
