module GoLite
  ( GoLiteError(..)
  , parse
  ) where

import qualified Language
import qualified Parser
import qualified Scanner
import qualified Weeder
import qualified TypeChecker
import qualified SymbolTable (initSymbolTable, SymbolTable)

--
data GoLiteError
  = ParserError String
  | WeederError [Weeder.WeederError]
  | TypeCheckerError TypeChecker.TypeCheckError
  deriving (Eq, Show)

--
parse :: FilePath -> String -> Either GoLiteError Language.Program
parse fp text =
  case Parser.parse fp text of
    Left errorMsg -> Left $ ParserError errorMsg
    Right program ->
      case Weeder.weed program of
        Nothing -> Right program
          -- case TypeChecker.typeCheck SymbolTable.initSymbolTable program of
          --            Left (_, symtbl) -> Right program
          --            Right err -> Left $ TypeCheckerError err
        Just weederErrors -> Left $ WeederError weederErrors
