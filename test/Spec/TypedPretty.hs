module Spec.TypedPretty where

import Control.Monad(forM_)
import Test.Hspec

import qualified Language
import qualified GoLite
import qualified Pretty.TypedPretty as TypedPretty
import qualified TypeChecker
import SymbolTable(SymbolTable(..))


spec :: [(String, String)] -> Spec
spec validTypeChecker =
  describe "Pretty" $
    forM_ validTypeChecker $ \(file, text) ->
      it ("correctly pretty prints : " ++ file) $
        case GoLite.parse file text of
          Right program -> case GoLite.typeCheck program of
            Right (_, SymbolTable _ history _) ->
              case GoLite.parse file $ TypedPretty.typedPretty program 0 history of
                Right program' -> program `shouldBe` program'
                Left parseError -> error "Pretty Print Error"
            Left typeError -> error "Type Checker Error"
          Left parseError -> error "Pretty Print Error"
