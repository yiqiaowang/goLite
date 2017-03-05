module Main where

import GoLite (parse)
import qualified Parser
import qualified Pretty
import System.Console.ArgParser
import System.FilePath
import System.IO (hPutStrLn, stderr)

data GoLiteOptions = GoLiteOptions
  { filename :: String
  } deriving (Show)

goLiteOptionsParser :: ParserSpec GoLiteOptions
goLiteOptionsParser =
  GoLiteOptions `parsedBy` reqPos "filename" `Descr`
  "goLite source file with relative file path"

main :: IO ()
main = do
  interface <- mkApp goLiteOptionsParser
  runApp interface compile

compile :: GoLiteOptions -> IO ()
compile options = do
  text <- readFile goLiteFile
  case parse goLiteFile text of
    Right program -> do
      writeFile prettyFile $ Pretty.pretty program 0
    Left errorMsg -> errorWithoutStackTrace $ show errorMsg
  where
    goLiteFile = filename options
    prettyFile = replaceExtension goLiteFile ".pretty.go"
