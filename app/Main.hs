module Main where

import qualified GoLite
import qualified Parser
import qualified Pretty
import System.Console.ArgParser
import System.FilePath
import Control.Monad(when)


--
data GoLiteOptions = GoLiteOptions
  { filename :: String
  , typeCheck :: Bool
  -- , dumpSymbolTable :: Bool
  -- , prettyPrintType :: Bool
  } deriving (Show)


--
goLiteOptionsParser :: ParserSpec GoLiteOptions
goLiteOptionsParser =
  GoLiteOptions
    `parsedBy` reqPos "filename" `Descr`
      "goLite source file with relative file path"
    `andBy` boolFlag "typecheck" `Descr`
      "Type checks the input source file"
    -- `andBy` boolFlag "dumpsymtab" `Descr`
    --   "Dumps top-most frame of the symbol table each time a scope is exited"
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
    Right program -> do
      -- Pretty print ast
      writeFile prettyFile $ Pretty.pretty program 0

      -- Typecheck if flag is passed
      when (typeCheck options) $
        case GoLite.typeCheck program of
          Right _ -> putStrLn "OK"
          Left typeError -> putStrLn "FAIL"
    Left parseError -> errorWithoutStackTrace $ show parseError

  where
    goLiteFile = filename options
    prettyFile = replaceExtension goLiteFile ".pretty.go"
