module Spec.Pretty where


import Control.Monad(forM_)
import Test.Hspec

import qualified Language.Language as Language
import qualified Parser.Parser as Parser
import qualified Pretty.Language as Pretty


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
