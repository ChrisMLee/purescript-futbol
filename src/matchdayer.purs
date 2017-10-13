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
import Data.Array (head, filter)
import Data.Maybe (Maybe(..))

pathToFile :: String
pathToFile = "./dummy_data/epl-fixtures.json"

-- fixtures <- pure $ jsonParser response.response >>= decodeJson

parseFixtures :: String -> Either String Fixtures
parseFixtures jsonString = jsonParser jsonString >>= decodeJson

type Date = String

filterFixturesByDate :: Date -> Fixtures -> Fixtures
filterFixturesByDate date fixtures = filter filterFixtures fixtures
  where
    filterFixtures :: Fixture ->  Boolean
    filterFixtures (Fixture f) = f.date == date

    returnFixtures :: Maybe Fixtures -> Fixtures
    returnFixtures (Just f) = f
    returnFixtures Nothing  = []

main :: forall e. Eff (fs :: FS, console :: CONSOLE, exception :: EXCEPTION | e) Unit
main = do
  result <- try $ readTextFile UTF8 pathToFile
  case result of
    Right jsonString ->
      either (\err -> logShow err) (\fixtures -> logShow $ filterFixturesByDate "2018-05-13T14:00:00Z" fixtures) (parseFixtures jsonString)
      -- This has a type of Either: you need to handle the either AND right a show instance for Fixture
      --jsonParser fixtures >>= decodeJson
    Left err ->
      logShow $ err

-- (try $ readTextFile UTF8 pathToFile) >>= logShow

