module Component (State, Query(..), ui, formatDate) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..), fromJust)
import Data.Either (Either(Right, Left), either)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)
import Data.String (take, drop)
import Data.Array (length, head)
import Partial.Unsafe (unsafePartial)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Optic.Core
import Data.Int(round)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import App.Types
import App.Lenses

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
                -- (fixtureComponent $ unsafePartial $ (fromJust $ head st.result))
                [HH.ul_ (map fixtureComponent st.result)]
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


getScore :: Maybe Number -> String
getScore Nothing = "Future"
getScore (Just x) = show $ round $ x

fixtureComponent :: forall s p i. Fixture -> H.HTML p i
fixtureComponent (Fixture f) = let
                                 hometeam  = f.homeTeamName
                                 homegoals = getScore $ f ^. result.._Result..goalsHomeTeam
                                 awayteam  = f.awayTeamName
                                 awaygoals = getScore $ f ^. result.._Result..goalsAwayTeam
                               in
                               HH.li_ [
                                         HH.div_ [ HH.text hometeam
                                                 , HH.text homegoals
                                                 ]
                                       , HH.div_ [ HH.text awayteam
                                                 , HH.text awaygoals
                                                 ]
                                       ]


