{-# LANGUAGE Strict #-}

-- A symbol table is a stack of stack frames
module SymbolTable
  ( SymbolTable(..)
  , SymbolTableError
  , Entry(..)
  , TypeCategory(..)
  , History
  , popFrame
  , popFrame'
  , addEntry
  , lookupIdentifier
  , newFrame
  , newFrameFunc
  , hasKey
  , initSymbolTable
  ) where

import Data.Map.Strict
       (Map, delete, insert, lookup, member, toList, fromList)
import qualified Data.Map.Strict as Map
import Language

data SymbolTableError
  = DuplicateIdentifier { duplicateIdentifier :: Identifier}
  | NotFoundIdentifier { notFoundIdentifier :: Identifier}
  | PopEmptyStackError
  | InconsistentTable
  deriving (Eq, Show)

type SymMap = Map Identifier Entry

data Entry = Entry
  { typeCategory :: TypeCategory
  , overWriteable :: Bool
  , dataType :: Maybe Type
  } deriving (Eq, Show)

data TypeCategory
  = CategoryVariable
  | CategoryType
  | CategoryAlias
  deriving (Eq, Show)

-- Create a new SymMap
newMap :: SymMap
newMap = Map.empty :: SymMap

-- Initial symbol table mapping
initMap :: SymMap
initMap =
  fromList
    [ (IdOrType "true", Entry CategoryVariable True $ Just (Alias "bool"))
    , (IdOrType "false", Entry CategoryVariable True $ Just (Alias "bool"))
    , (IdOrType "int", Entry CategoryType True $ Just (Alias "int"))
    , (IdOrType "float64", Entry CategoryType True $ Just (Alias "float64"))
    , (IdOrType "rune", Entry CategoryType True $ Just (Alias "rune"))
    , (IdOrType "bool", Entry CategoryType True $ Just (Alias "bool"))
    , (IdOrType "string", Entry CategoryType True $ Just (Alias "string"))
    , (IdOrType "print", Entry CategoryVariable False $ Just (BuiltIn))
    , (IdOrType "println", Entry CategoryVariable False $ Just (BuiltIn))
    , (IdOrType "append", Entry CategoryVariable False $ Just (BuiltIn))
    ]

-- Check if an idname is in the symbol table
hasKey :: SymMap -> Identifier -> Bool
hasKey map key = Map.member key map

-- Add new entry to symbol table
addSym :: Identifier -> Entry -> SymMap -> SymMap
addSym id entry map = Map.insert id entry map

-- Get an entry from SymMap
getSym :: Identifier -> SymMap -> Maybe Entry
getSym name map = Map.lookup name map

-- SymbolTable type
type Stack = [SymMap]

type History = [Stack]

type ContextRecord = [SymMap]

data SymbolTable =
  SymbolTable Stack
              [History]
              ContextRecord
  deriving (Eq, Show)

emptySymbolTable = SymbolTable [] [] [] :: SymbolTable

initSymbolTable = SymbolTable [initMap] [] [] :: SymbolTable

newFrame :: SymbolTable -> SymbolTable
newFrame (SymbolTable xs h c) = SymbolTable (newMap : xs) h c

newFrameFunc :: SymbolTable -> SymbolTable
newFrameFunc (SymbolTable xs h c) = SymbolTable (newMap : xs) ([]:h) c

popFrame :: SymbolTable -> Either SymbolTableError (SymMap, SymbolTable)
popFrame (SymbolTable s@(m:ms) h c) =
  case null s of
    True -> Left PopEmptyStackError
    False -> Right (m, SymbolTable ms h c)


popFrame' :: SymbolTable -> Either SymbolTableError (SymMap, SymbolTable)
popFrame' (SymbolTable s@(m:ms) (h:hs) c) =
  case null s of
    True -> Left PopEmptyStackError
    False -> Right (m, SymbolTable ms ((s : h):hs) (m : c))
popFrame' (SymbolTable s@(m:ms) [] c) =
  case null s of
    True -> Left PopEmptyStackError
    False -> Right (m, SymbolTable ms [[s]] (m : c))

addEntry :: SymbolTable
         -> Identifier
         -> Entry
         -> Either SymbolTableError SymbolTable
addEntry s@(SymbolTable ss h c) i e =
  if isBlank i
    then Right s
    else
  case popFrame s of
    Right (f, symtbl) ->
      if hasKey f i
        then case lookupIdentifier s i of
               Right (Entry _ re _) -> if not re
                                       then Left (DuplicateIdentifier i)
                                       else Right (SymbolTable ((addSym i e $ head ss) : (tail ss)) h c)
        else Right (SymbolTable ((addSym i e $ head ss) : (tail ss)) h c)
    Left err -> Left err


isBlank :: Identifier -> Bool
isBlank (IdOrType "_") = True
isBlank (IdArray "_" _) = True
isBlank _ = False



-- Lookup an entry in the symbol table
lookupIdentifier :: SymbolTable -> Identifier -> Either SymbolTableError Entry
lookupIdentifier s@(SymbolTable ss h c) i =
  if null ss
    then Left $ NotFoundIdentifier i
    else case popFrame s of
           Right (f, symtbl) ->
             if hasKey f i
               then case (getSym i f) of
                      (Nothing) -> Left InconsistentTable
                      (Just e) -> Right e
               else lookupIdentifier symtbl i
           Left err -> Left err
