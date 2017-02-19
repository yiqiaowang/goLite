module Spec.Pretty where


import Control.Monad(forM_)
import Test.Hspec

import qualified Language
import qualified Parser
import qualified Pretty


spec :: [(String, String)] -> Spec
spec validSyntax =
  describe "Pretty" $ do
    forM_ validSyntax $ \(file, text) ->
      it ("correctly pretty prints : " ++ file) $
        case Parser.parse file text of
          Right program -> case Parser.parse file $ Pretty.pretty program of
            Right program' -> program `shouldBe` program'
            Left parseError -> error "shit"
          Left parseError -> error "fuck"