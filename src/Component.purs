module Component (State, Fixtures, Fixture, Query(..), ui, formatDate) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..), fromJust)
import Data.Either (Either(Right, Left))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)
import Data.String (take, drop)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, head)
import Partial.Unsafe (unsafePartial)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Optic.Core
import Data.Int(round)

-- Things to put in config file (monad transformers):
-- season
-- competitions

type State =
  { loading :: Boolean
  , date :: String
  , result :: Fixtures
  }

data Query a
  = SetDate String a
  | MakeRequest a

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
  Result { goalsHomeTeam :: Number
         , goalsAwayTeam :: Number
         }

instance decodeJsonResult :: DecodeJson Result where
  decodeJson json = do
    obj <- decodeJson json
    goalsHomeTeam <- obj .? "goalsHomeTeam"
    goalsAwayTeam <- obj .? "goalsAwayTeam"
    pure $ Result { goalsHomeTeam: goalsHomeTeam, goalsAwayTeam: goalsAwayTeam }

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

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, date: "", result: [] }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ HH.h1_ [ HH.text "Lookup Fixtures" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter date:" ]
          , HH.input
              [ HP.value st.date
              , HE.onValueInput (HE.input SetDate)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeRequest)
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
            case length $ st.result of
              0 ->
                  [ HH.div_ [HH.text "hi"]]
              _ ->
                  -- [ HH.div_ [HH.text "hi"]]
                  [(fixtureComponent $ unsafePartial $ (fromJust $ head st.result))]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (AppEffects eff))
  eval = case _ of
    SetDate date next -> do
      H.modify (_ { date = date, result = [] })
      pure next
    MakeRequest next -> do
      date <- H.gets _.date
      H.modify (_ { loading = true })
      formattedDate <- pure $ formatDate date
      response <- H.liftAff $ AX.get ("http://localhost:8080/fixtures/445/2017/" <> formattedDate <> "/" <> formattedDate)
      log' $ response.response
      let receiveFixtures (Right x) = H.modify (_ { loading = false, result = x })
          receiveFixtures (Left err) = do
            log' $ err
            H.modify (_ { loading = false, result = [] })
      fixtures <- pure $ jsonParser response.response >>= decodeJson
      receiveFixtures $ fixtures
      pure next

log' = H.liftAff <<< log

formatDate :: String -> String
formatDate x = (getYear x) <> "-" <> (getDay x) <> "-" <> (getMonth x) where
               getYear  = drop 4
               getMonth = (take 2) <<< (drop 2)
               getDay   = take 2


fixtureComponent :: forall s p i. Fixture -> H.HTML p i
fixtureComponent (Fixture f) = let
                                 hometeam  = f.homeTeamName
                                 homegoals = show $ round $ f ^. result.._Result..goalsHomeTeam
                                 awayteam  = f.awayTeamName
                                 awaygoals = show $ round $ f ^. result.._Result..goalsAwayTeam
                               in
                               HH.div_ [
                                         HH.div_ [ HH.text hometeam
                                                 , HH.text homegoals
                                                 ]
                                       , HH.div_ [ HH.text awayteam
                                                 , HH.text awaygoals
                                                 ]
                                       ]


-- Lenses

_Fixture :: Lens' Fixture {_links:: LinkGroup, date :: String, status :: String, matchday :: Number, homeTeamName :: String, awayTeamName :: String, result:: Result}
_Fixture f (Fixture b) = Fixture <$> f b

_Link :: Lens' Link { href :: String}
_Link f (Link b) = Link <$> f b

_LinkGroup :: Lens' LinkGroup {self :: Link, competition :: Link, homeTeam :: Link, awayTeam :: Link}
_LinkGroup f (LinkGroup b) = LinkGroup <$> f b

_Result :: Lens' Result {goalsHomeTeam :: Number, goalsAwayTeam :: Number}
_Result f (Result b) = Result <$> f b

result :: forall a b r. Lens { result :: a | r } { result :: b | r } a b
result = lens _.result (_ { result = _ })

goalsHomeTeam :: forall b a r. Lens {goalsHomeTeam :: a | r} {goalsHomeTeam :: b | r} a b
goalsHomeTeam = lens _.goalsHomeTeam (_ { goalsHomeTeam = _})

goalsAwayTeam :: forall b a r. Lens {goalsAwayTeam :: a | r} {goalsAwayTeam :: b | r} a b
goalsAwayTeam = lens _.goalsAwayTeam (_ { goalsAwayTeam = _})
