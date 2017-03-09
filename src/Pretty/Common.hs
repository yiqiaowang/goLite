module Pretty.Common where


import Pretty.Pretty
import Language.Common
import Data.Char (chr)

--
instance Pretty Literal where
  pretty (Int' i) _ = show i
  pretty (Float64 f) _ = show f
  pretty (Rune i) _ = (show . chr . fromIntegral) i
  pretty (String s) _ = s
  pretty (Raw s) _ = s
