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

-- Where the First Type is the variable name and the second
-- Type is the Associated Type
data Identifier
  = IdOrType String
  | IdArray String [Int]
  | IdField [Identifier]
  deriving (Eq, Ord, Show)

--
data Type
  = Alias String
  | Array Type Int
  | Slice Type
  | Struct [([Identifier], Type)]
  | Func [Type] (Maybe Type)
  | BuiltIn
  deriving (Eq, Ord, Show)

-- Parameter data type (List of identifiers with an associated type)
data Parameter =
  Parameter [Identifier] Type
  deriving (Eq, Show)

-- Type aliasing
data TypeName =
  TypeName Type Type
  deriving (Eq, Show)
