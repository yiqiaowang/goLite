module Spec.Pretty where


import Control.Monad(forM_)
import Test.Hspec

import qualified Language
import qualified Parser
import qualified Pretty.Pretty as Pretty


spec :: [(String, String)] -> Spec
spec validSyntax =
  describe "Pretty" $ do
    forM_ validSyntax $ \(file, text) ->
      it ("correctly pretty prints : " ++ file) $
        case Parser.parse file text of
          Right program -> case Parser.parse file $ Pretty.pretty program 0 of
            Right program' -> program `shouldBe` program'
            Left parseError -> error "Pretty Print Error"
          Left parseError -> error "Pretty Print Error"
