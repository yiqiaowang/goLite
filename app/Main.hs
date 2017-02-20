module Main where


import System.IO(hPutStrLn, stderr)
import System.FilePath
import System.Console.ArgParser
import qualified Parser
import qualified Pretty


data GoLiteOptions = GoLiteOptions
  { filename :: String }
  deriving (Show)


goLiteOptionsParser :: ParserSpec GoLiteOptions
goLiteOptionsParser = GoLiteOptions
  `parsedBy` reqPos "filename" `Descr` "goLite source file with relative file path"


main :: IO ()
main = do
  interface <- mkApp goLiteOptionsParser
  runApp interface compile


compile :: GoLiteOptions -> IO ()
compile options = do
  text <- readFile goLiteFile
  case Parser.parse goLiteFile text of
    Right program -> writeFile prettyFile $ Pretty.pretty program
    Left errorMsg -> hPutStrLn stderr errorMsg

  where
    goLiteFile = filename options
    prettyFile = replaceExtension goLiteFile ".pretty.go"
