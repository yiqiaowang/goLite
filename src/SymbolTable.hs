-- A symbol table is a stack of stack frames
module SymbolTable
  ( SymbolTable
  , SymbolTableError
  , newSymbolTable
  , newFrame
  , popFrame
  , pushFrame
  , topFrame
  ) where

import Data.Map.Strict
       (Map, adjust, insert, lookup, member, toList)
import qualified Data.Map.Strict as Map
import Language (Identifier, Type)

data SymbolTableError
  = DuplicateIdentifier { duplicateIdentifier :: String }
  | NotFoundIdentifier { notFoundIdentifier :: String }
  | InconsistentTable
  deriving (Eq, Show)

type SymMap = Map String Entry

data Entry = Entry
  { typeCategory :: TypeCategory
  , dataType :: Type
  } deriving (Eq, Show)

data TypeCategory
  = CategoryIdentifier
  | CategoryType 
  deriving (Eq, Show)


-- Create a new SymMap
newMap :: SymMap
newMap = Map.empty :: SymMap

-- Check if an idname is in the symbol table
hasKey :: String -> SymMap -> Bool
hasKey key map = Map.member key map

-- -- Add new entry to symbol table
addSym :: String -> Entry -> SymMap -> SymMap
addSym id entry map = Map.insert id entry map

-- -- Get an entry from the symbol table
getSym :: String -> SymMap -> Maybe Entry
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

newSymbolTable = SymbolTable empty :: SymbolTable

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
         -> String
         -> Entry 
         -> (SymbolTable, Maybe SymbolTableError)
addEntry s i e =
  if hasKey i (getMap $ fst $ popFrame s)
    then (s, Just (DuplicateIdentifier i))
    else (s', Nothing)
  where
    s' = pushFrame p (Frame $ addSym i e (getMap f))
    f = fst $ popFrame s
    p = snd $ popFrame s

-- Lookup an entry in the symbol table
lookupIdentifier :: SymbolTable
                 -> String
                 -> Either Entry SymbolTableError
lookupIdentifier s i =
  if isEmpty $ getStack s
    then Right $ NotFoundIdentifier i
    else if hasKey i (getMap t)
           then case (getSym i (getMap t)) of
                 (Nothing) -> Right InconsistentTable
                 (Just e) -> Left e
           else lookupIdentifier s' i
  where
    t = fst $ popFrame s
    s' = snd $ popFrame s
