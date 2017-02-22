module GoLite
  ( parse
  ) where


import qualified Parser
import qualified Scanner
import qualified Weeder
import qualified Language


parse :: FilePath -> String -> Either String Language.Program
parse fp text =
  case Parser.parse fp text of
    Left errorMsg -> Left errorMsg
    Right program -> case Weeder.weed program of
      Nothing -> return program
      Just weederError -> Left $ show weederError
