module Main where

import Control.Monad (when)
import qualified GoLite
import qualified Parser
import qualified Pretty.Pretty as Pretty
import qualified Pretty.TypedPretty as TypedPretty

import System.Console.ArgParser
import System.FilePath
import qualified Text.Show.Pretty as Pr
import SymbolTable
import Language (Identifier)
import Data.Map.Strict (toList)


--
data GoLiteOptions = GoLiteOptions
  { filename :: String
  , typeCheck :: Bool
  , dumpSymbolTable :: Bool
  , dumpAST :: Bool
  , prettyPrintType :: Bool
  } deriving (Show)


--
goLiteOptionsParser :: ParserSpec GoLiteOptions
goLiteOptionsParser = GoLiteOptions
  `parsedBy` reqPos "filename" `Descr`
    "goLite source file with relative file path"
  `andBy` boolFlag "typecheck" `Descr`
    "Type checks the input source file"
  `andBy` boolFlag "dumpsymtab" `Descr`
    "Dumps the entire symbol table on completion or error."
  `andBy` boolFlag "astdump" `Descr`
    "Dumps the ast on completed parse"
  `andBy` boolFlag "pptype" `Descr`
    "Pretty prints the program with the type of each expression"

--
main :: IO ()
main = do
  interface <- mkApp goLiteOptionsParser
  runApp interface processFile

processFile :: GoLiteOptions -> IO ()
processFile options = do
  text <- readFile goLiteFile
  case GoLite.parse goLiteFile text of
    Right program -> do
      writeFile prettyFile $ Pretty.pretty program 0

      -- Typecheck if flag is passed
      when (typeCheck options) $
        case GoLite.typeCheck program of
          Right (_, SymbolTable _ history _) -> do
            putStrLn "OK"
            when (prettyPrintType options) $
              writeFile ppTypeFile $ TypedPretty.typedPretty program 0 history
          Left (GoLite.TypeCheckerError (err, symtbl)) ->
            errorWithoutStackTrace ("FAIL\n" ++ Pr.ppShow err)

      -- Dump symboltable
      when (dumpSymbolTable options) $
        case GoLite.typeCheck program of
          Right (_, SymbolTable.SymbolTable s h c) ->
            putStrLn $ draw $ reverse $ fmap (toList . getMap) c
          Left (GoLite.TypeCheckerError (_, SymbolTable.SymbolTable s h c)) ->
            errorWithoutStackTrace $ draw $ reverse $ fmap (toList . getMap) c

      -- Dump ast
      when (dumpAST options) $
        putStrLn $ Pr.ppShow program

    Left parseError -> errorWithoutStackTrace $ Pr.ppShow parseError

  where
    goLiteFile = filename options
    prettyFile = replaceExtension goLiteFile ".pretty.go"
    ppTypeFile = replaceExtension goLiteFile ".pptype.go"


-- Pretty Print Stack Frames
leftPad :: Int -> String -> String
leftPad t x = if 0 < k then replicate k ' '
                               else "\t"
                                    where k = (t - length x)

drawRow :: (Identifier, Entry) -> String
drawRow (i,e) = (show i) ++ (leftPad 30 (show i))  ++  (show e) ++ "\n"

-- print lists of records: a header, then each row
draw :: [[(Identifier,Entry)]] -> String
draw [] = "Done."
draw (x:xs) =
   "Context:\nIdentifier" ++ leftPad 30 "Identifier" ++ "Mapping\n" ++
   concatMap drawRow x ++ "\n" ++ draw xs
