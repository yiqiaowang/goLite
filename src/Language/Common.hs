module Language.Common where


-- Packages, Identifiers, and Types are all strings
type Package = String
type FunctionName = String


-- Literal values
data Literal
  = Int' Int
  | Float64 Float
  | Rune Int
  | String String
  | Raw String
  deriving (Eq, Ord, Show)
