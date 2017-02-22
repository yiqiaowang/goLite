module Spec.GoLite where


import Control.Monad(forM_)
import Test.Hspec
import Test.Hspec.Core.Runner(hspecResult)
import Data.Either(isLeft, isRight)
import Control.Monad(forM_)


import GoLite


spec :: [(String, String)] -> [(String, String)] -> Spec
spec valid invalid =
  describe "GoLite" $ do
    forM_ valid $ \(file, text) ->
      it ("correctly parses and weeds : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isRight

    forM_ invalid $ \(file, text) ->
      it ("correctly parses but fails to weed : " ++ file) $
        GoLite.parse file text `shouldSatisfy` isLeft
