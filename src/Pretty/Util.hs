module Pretty.Util where


import Pretty.Pretty
import Data.List(replicate, intercalate)


--
commaSepList :: Pretty a => [a] -> Integer -> String
commaSepList string i = intercalate ", " (map (`pretty` i) string)

--
spacePrint :: Integer -> String
spacePrint x = replicate (fromInteger x) '\t'

--
wrapSquare :: String -> String
wrapSquare s = "[" ++ s ++ "]"

--
wrapSquareList :: Pretty a => [a] -> Integer -> String
wrapSquareList xs i = concatMap wrapSquare (map (`pretty` i) xs)

--
dotSepList :: [String] -> String
dotSepList = intercalate "."
