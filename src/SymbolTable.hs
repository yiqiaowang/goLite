-- A symbol table is a stack of stack frames
module SymbolTable
  ( SymbolTable
  , SymbolTableError
  , Entry (..)
  , TypeCategory (..)
  , initSymbolTable
  , newFrame
  , popFrame
  , pushFrame
  , topFrame
  , addEntry
  , lookupIdentifier
  , hasKey
  , getMap
  ) where

import Data.Map.Strict
       (Map, adjust, insert, lookup, member, toList, fromList)
import qualified Data.Map.Strict as Map
import Language

data SymbolTableError
  = DuplicateIdentifier { duplicateIdentifier :: Identifier }
  | NotFoundIdentifier { notFoundIdentifier :: Identifier }
  | InconsistentTable
  deriving (Eq, Show)

type SymMap = Map Identifier Entry

data Entry = Entry
  { typeCategory :: TypeCategory
  , dataType :: Maybe Type
  } deriving (Eq, Show)

data TypeCategory
  = CategoryVariable
  | CategoryType 
  deriving (Eq, Show)


-- Create a new SymMap
newMap :: SymMap
newMap = Map.empty :: SymMap


-- Initial symbol table mapping
initMap :: SymMap
initMap = fromList[(IdOrType "true", Entry CategoryVariable $ Just Bool)
                  ,(IdOrType "false", Entry CategoryVariable $ Just Bool)
                  ,(IdOrType "int", Entry CategoryType $ Just (Alias "int"))
                  ,(IdOrType "float64", Entry CategoryType $ Just (Alias "float64"))
                  ,(IdOrType "rune", Entry CategoryType $ Just (Alias "rune"))
                  ,(IdOrType "bool", Entry CategoryType $ Just (Alias "bool"))
                  ,(IdOrType "string", Entry CategoryType $ Just (Alias "string"))
                  ,(IdOrType "print", Entry CategoryVariable $ Just (Func))
                  ,(IdOrType "println", Entry CategoryVariable $ Just (Func))
                  ,(IdOrType "append", Entry CategoryVariable $ Just (Func))]

  
-- Check if an idname is in the symbol table
hasKey :: SymMap -> Identifier -> Bool
hasKey map key = Map.member key map

-- -- Add new entry to symbol table
addSym :: Identifier -> Entry -> SymMap -> SymMap
addSym id entry map = Map.insert id entry map

-- -- Get an entry from SymMap
getSym :: Identifier -> SymMap -> Maybe Entry
getSym name map = Map.lookup name map

newtype Frame =
  Frame SymMap
  deriving (Eq, Show)


-- Stack to hold the frames
newtype Stack =
  Stack [Frame]
  deriving (Eq, Show)

empty :: Stack
empty = Stack []

isEmpty :: Stack -> Bool
isEmpty (Stack s) = null s

push :: Frame -> Stack -> Stack
push f (Stack fs) = Stack (f : fs)

top :: Stack -> Frame
top (Stack s) = head s

pop :: Stack -> (Frame, Stack)
pop (Stack (s:ss)) = (s, Stack ss)

getMap :: Frame -> SymMap
getMap (Frame m) = m

getStack :: SymbolTable -> Stack
getStack (SymbolTable s) = s

newtype SymbolTable =
  SymbolTable Stack
  deriving (Eq, Show)

emptySymbolTable = SymbolTable empty :: SymbolTable

initSymbolTable = pushFrame emptySymbolTable (Frame initMap) :: SymbolTable

newFrame :: SymbolTable -> SymbolTable
newFrame (SymbolTable s) = SymbolTable (push (Frame newMap) s)

popFrame :: SymbolTable -> (Frame, SymbolTable)
popFrame (SymbolTable s) = (fst $ pop s, SymbolTable (snd $ pop s))

pushFrame :: SymbolTable -> Frame -> SymbolTable
pushFrame (SymbolTable s) f = SymbolTable (push f s)

topFrame :: SymbolTable -> Maybe Frame
topFrame (SymbolTable s) =
  if isEmpty s
    then Nothing
    else Just $ top s

-- Adds an entry to the top frame
addEntry :: SymbolTable
         -> Identifier
         -> Entry 
         -> Either SymbolTableError SymbolTable 
addEntry s i e =
  if hasKey f i
    then Left (DuplicateIdentifier i)
    else Right s'
  where
    s' = pushFrame p (Frame $ addSym i e f)
    f = getMap $ fst $ popFrame s
    p = snd $ popFrame s

-- Lookup an entry in the symbol table
lookupIdentifier :: SymbolTable
                 -> Identifier
                 -> Either SymbolTableError Entry 
lookupIdentifier s i =
  if isEmpty $ getStack s
    then Left $ NotFoundIdentifier i
    else if hasKey (getMap t) i 
           then case (getSym i (getMap t)) of
                 (Nothing) -> Left InconsistentTable
                 (Just e) -> Right e
           else lookupIdentifier s' i
  where
    t = fst $ popFrame s
    s' = snd $ popFrame s
