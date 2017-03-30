module CodeGen.Codeable where


import Data.List(intercalate)
import Language

class Codeable a where
  code :: a -> Integer -> String

  codeList :: [a] -> Integer -> String
  codeList ps i = concatMap (`code` i) ps


--
commaSepList :: Codeable a => [a] -> Integer -> String
commaSepList string i = intercalate ", " (map (`code` i) string)

--
spacePrint :: Integer -> String
spacePrint x = replicate (fromInteger x) '\t'

--
wrapSquare :: String -> String
wrapSquare s = "[" ++ s ++ "]"

--
wrapSquareList :: Codeable a => [a] -> Integer -> String
wrapSquareList xs i = concatMap wrapSquare (map (`code` i) xs)

--
dotSepList :: [String] -> String
dotSepList = intercalate "."
