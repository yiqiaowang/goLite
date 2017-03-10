module Main where

import Control.Monad (when)
import qualified GoLite
import qualified Parser
import qualified Pretty
import System.Console.ArgParser
import System.FilePath
import qualified Text.Show.Pretty as Pr
--
data GoLiteOptions = GoLiteOptions
  { filename :: String
  , typeCheck :: Bool
  , dumpSymbolTable :: Bool
  , dumpAST :: Bool
  -- , prettyPrintType :: Bool
  } deriving (Show)

--
goLiteOptionsParser :: ParserSpec GoLiteOptions
goLiteOptionsParser =
  GoLiteOptions `parsedBy` reqPos "filename" `Descr`
  "goLite source file with relative file path" `andBy`
  boolFlag "typecheck" `Descr`
  "Type checks the input source file" `andBy`
  boolFlag "symtbldump" `Descr`
  "Dumps the entire symbol table on completion or error." `andBy`
  boolFlag "astdump" `Descr`
  "Dumps the ast on completed parse"
    -- `andBy` boolFlag "pptype" `Descr`
    --   "Pretty prints the program with the type of each expression"

--
main :: IO ()
main = do
  interface <- mkApp goLiteOptionsParser
  runApp interface processFile

processFile :: GoLiteOptions -> IO ()
processFile options = do
  text <- readFile goLiteFile
  case GoLite.parse goLiteFile text of
    Right program
      -- Pretty print program
     -> do
      writeFile prettyFile $ Pretty.pretty program 0
      -- Typecheck if flag is passed
      when (typeCheck options) $
        case GoLite.typeCheck program of
          Right _ -> putStrLn "OK"
          Left (GoLite.TypeCheckerError (err, symtbl)) -> putStrLn ("FAIL\n" ++ Pr.ppShow err)
      -- Dump symboltable
      when (dumpSymbolTable options) $
        case GoLite.typeCheck program of
          Right (_, symtbl) -> putStrLn $ Pr.ppShow symtbl
          Left (GoLite.TypeCheckerError (_, symtbl)) -> putStrLn $ Pr.ppShow symtbl
      -- Dump ast
      when (dumpAST options) $
        putStrLn $ Pr.ppShow program
    Left parseError -> errorWithoutStackTrace $ Pr.ppShow parseError
  where
    goLiteFile = filename options
    prettyFile = replaceExtension goLiteFile ".pretty.go"
