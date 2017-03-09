module GoLite
  ( GoLiteError(..)
  , parse
  , typeCheck
  ) where

import qualified Language.Language as Language
import qualified Parser.Parser as Parser
import qualified Parser.Scanner as Scanner
import qualified Parser.Weeder as Weeder
import qualified TypeChecker.TypeChecker as TypeChecker
import qualified TypeChecker.SymbolTable as SymbolTable
  (initSymbolTable, SymbolTable)


--
data GoLiteError
  = ParserError String
  | WeederError [Weeder.WeederError]
  | TypeCheckerError (TypeChecker.TypeCheckError, SymbolTable.SymbolTable)
  deriving (Eq, Show)


--
parse :: FilePath -> String -> Either GoLiteError Language.Program
parse fp text =
  case Parser.parse fp text of
    Left errorMsg -> Left $ ParserError errorMsg
    Right program ->
      case Weeder.weed program of
        Nothing -> Right program
        Just weederErrors -> Left $ WeederError weederErrors


--
typeCheck
  :: Language.Program
  -> Either GoLiteError (Maybe Language.Type, SymbolTable.SymbolTable)
typeCheck ast =
  case TypeChecker.typeCheck SymbolTable.initSymbolTable ast of
    Left typeError -> Left $ TypeCheckerError typeError
    Right r -> Right r
