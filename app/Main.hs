module Main where

import Control.Monad (when, unless)
import qualified GoLite
import qualified Parser
import qualified Pretty.Pretty as Pretty
import qualified Pretty.TypedPretty as TypedPretty
import qualified CodeGen.CodeGenerator as Generator
import qualified CodeGen.Prefix as Prefix

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
  , prettyPrint :: Bool
  , codeGen :: Bool
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
  `andBy` boolFlag "regularpp" `Descr`
    "Pretty prints the program"
  `andBy` boolFlag "codegen" `Descr`
    "Generates input code in target language."


--
main :: IO ()
main = do
  let interface = (mkDefaultApp goLiteOptionsParser "golite") { getAppVersion = Just "1.0.0" }
  runApp interface processFile

processFile :: GoLiteOptions -> IO ()
processFile options = do
  text <- readFile goLiteFile



  -- output pretty file
  when (prettyPrint options) $
    case GoLite.parse goLiteFile text of
      Right program -> do
        putStrLn "OK"
        writeFile prettyFile $ Pretty.pretty program 0
      Left parseError -> do
        putStrLn "FAIL"
        errorWithoutStackTrace $ Pr.ppShow parseError

  -- dump ast
  when (dumpAST options) $
    case GoLite.parse goLiteFile text of
      Right program -> do
        putStrLn "OK"
        putStrLn $ Pr.ppShow program 
      Left parseError -> do
        putStrLn "FAIL"
        errorWithoutStackTrace $ Pr.ppShow parseError

  -- output pretty file with types
  when (prettyPrintType options) $
    case GoLite.parse goLiteFile text of
      Right program ->
        case GoLite.typeCheck program of
          Right (_, SymbolTable _ history _) -> do
            putStrLn "OK"
            writeFile ppTypeFile $ TypedPretty.prettyPrintProgram program 0 history
          Left (GoLite.TypeCheckerError (err,_)) ->
            errorWithoutStackTrace ("FAIL\n" ++ Pr.ppShow err)
      Left parseError -> errorWithoutStackTrace $ Pr.ppShow parseError

  -- TypeCheck program
  when (typeCheck options) $
    case GoLite.parse goLiteFile text of
      Right program ->
        case GoLite.typeCheck program of
          Right (_, SymbolTable _ history _) -> 
            putStrLn "OK"
          Left (GoLite.TypeCheckerError (err,_)) ->
            errorWithoutStackTrace ("FAIL\n" ++ Pr.ppShow err)
      Left parseError -> errorWithoutStackTrace $ Pr.ppShow parseError
  
  -- dumpsymtab
  when (dumpSymbolTable options) $
    case GoLite.parse goLiteFile text of
      Right program ->
        case GoLite.typeCheck program of
          Right (_, SymbolTable _ _ c) -> do
            putStrLn "OK"
            putStrLn $ draw $ reverse $ fmap toList c
          Left (GoLite.TypeCheckerError (err, SymbolTable.SymbolTable _ _ c)) -> do
            putStrLn "FAIL"
            errorWithoutStackTrace $ draw $ reverse $ fmap toList c
      Left parseError -> errorWithoutStackTrace $ Pr.ppShow parseError


  -- code gen
  when (codeGen options) $
    case GoLite.parse goLiteFile text of
      Right program ->
        case GoLite.typeCheck program of
          Right (_, SymbolTable _ h _) -> do
            putStrLn "OK"
            writeFile jsFile $ Generator.codeProgram program 0 h
          Left (GoLite.TypeCheckerError (err, _)) ->
            errorWithoutStackTrace ("FAIL\n" ++ Pr.ppShow err)
      Left parseError -> errorWithoutStackTrace $ Pr.ppShow parseError


  where
    goLiteFile = filename options
    prettyFile = replaceExtension goLiteFile ".pretty.go"
    ppTypeFile = replaceExtension goLiteFile ".pptype.go"
    jsFile = replaceExtension goLiteFile ".js"


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
