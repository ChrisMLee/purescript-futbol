module App.Types
  ( AppEffects
  , Link (..)
  , Result (..)
  , LinkGroup (..)
  , Fixture (..)
  , Fixtures
  ) where

import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?), (.??))
import Data.Argonaut.Parser (jsonParser)
import Control.Monad.Aff (Aff)
import Network.HTTP.Affjax as AX
import Data.Maybe (Maybe(..))
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)

type AppEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  , ajax :: AX.AJAX
  | eff)

newtype Link =
  Link { href :: String }

instance decodeJsonLink :: DecodeJson Link where
  decodeJson json = do
    obj <- decodeJson json
    href <- obj .? "href"
    pure $ Link { href: href }

newtype Result =
  Result { goalsHomeTeam :: Maybe Number
         , goalsAwayTeam :: Maybe Number
         }

instance decodeJsonResult :: DecodeJson Result where
  decodeJson json = do
    obj <- decodeJson json
    goalsHomeTeam <- obj .? "goalsHomeTeam"
    goalsAwayTeam <- obj .? "goalsAwayTeam"
    pure $ Result { goalsHomeTeam, goalsAwayTeam }

newtype LinkGroup =
  LinkGroup  { self :: Link
             , competition :: Link
             , homeTeam:: Link
             , awayTeam:: Link
             }

instance decodeJsonLinkGroup :: DecodeJson LinkGroup where
  decodeJson json = do
    obj <- decodeJson json
    self <- obj .? "self"
    competition <- obj .? "competition"
    homeTeam <- obj .? "homeTeam"
    awayTeam <- obj .? "awayTeam"
    pure $  LinkGroup { self: self, competition: competition, homeTeam: homeTeam, awayTeam: awayTeam }


newtype Fixture =
  Fixture { _links:: LinkGroup
            , date:: String
            , status:: String
            , matchday:: Number
            , homeTeamName:: String
            , awayTeamName:: String
            , result :: Result
          }

type Fixtures = Array Fixture

instance decodeJsonFixture :: DecodeJson Fixture where
  decodeJson json = do
    obj <- decodeJson json
    _links <- obj .? "_links"
    date <- obj .? "date"
    status <- obj .? "status"
    matchday <- obj .? "matchday"
    homeTeamName <- obj .? "homeTeamName"
    awayTeamName <- obj .? "awayTeamName"
    result <- obj .? "result"
    pure $ Fixture {_links: _links, date: date, status: status, matchday: matchday, homeTeamName: homeTeamName, awayTeamName: awayTeamName, result: result}