module CodeGen.Codeable where


import Data.List(intercalate)
import Language
import SymbolTable

class Codeable a where
  code :: a -> Integer -> History -> String

  codeList :: [a] -> Integer -> History -> String
  codeList cs i h = concatMap (\c -> code c i h) cs


--
commaSepList :: Codeable a => [a] -> Integer -> History -> String
commaSepList cs i h = intercalate ", " (map (\c -> code c i h) cs)

--
spacePrint :: Integer -> String
spacePrint x = replicate (fromInteger x) '\t'

--
wrapSquare :: String -> String
wrapSquare s = "[" ++ s ++ "]"

--
wrapSquareList :: Codeable a => [a] -> Integer -> History -> String
wrapSquareList cs i h = concatMap (wrapSquare . (\c -> code c i h)) cs

--
dotSepList :: [String] -> String
dotSepList = intercalate "."