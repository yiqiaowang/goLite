module GoLite
  ( GoLiteError(..)
  , parse
  ) where

import qualified Language
import qualified Parser
import qualified Scanner
import qualified Weeder

--
data GoLiteError
  = ParserError String
  | WeederError [Weeder.WeederError]
  deriving (Eq, Show)

--
parse :: FilePath -> String -> Either GoLiteError Language.Program
parse fp text =
  case Parser.parse fp text of
    Left errorMsg -> Left $ ParserError errorMsg
    Right program ->
      case Weeder.weed program of
        Nothing -> return program
        Just weederErrors -> Left $ WeederError weederErrors
