module Matchdayer where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION, try)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import App.Types
import App.Lenses
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Either (Either(Right, Left), either)

pathToFile :: String
pathToFile = "./dummy_data/epl-fixtures.json"

-- fixtures <- pure $ jsonParser response.response >>= decodeJson

-- parseFixtures :: Either Error Fixtures
-- parseFixtures =
--   case parsed of
--     Right 


main :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = do
  result <- try $ readTextFile UTF8 pathToFile
  case result of
    Right fixtures ->
      -- This has a type of Either: you need to handle the either AND right a show instance for Fixture
      --jsonParser fixtures >>= decodeJson
    Left err ->
      logShow $ err

-- (try $ readTextFile UTF8 pathToFile) >>= logShow

