module Spec.TypeChecker where


import Control.Monad(forM_)
import Test.Hspec
import Test.Hspec.Core.Runner(hspecResult)
import Data.Either(isLeft, isRight)


import GoLite


spec :: [(String, String)] -> [(String, String)] -> Spec
spec invalidTypeChecker validTypeChecker = do
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
    isTypeError (Left (TypeCheckerError _)) = True
    isTypeError _ = False
