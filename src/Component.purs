module Component (State, Query(..), ui) where

import Prelude
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Control.Monad.Aff.Console (CONSOLE, log)
import DOM (DOM)

type State =
  { loading :: Boolean
  , date :: String
  , result :: Maybe String
  }

data Query a
  = SetDate String a
  | MakeRequest a

type AppEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  , ajax :: AX.AJAX
  | eff)


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
  initialState = { loading: false, date: "", result: Nothing }

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
          case st.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (AppEffects eff))
  eval = case _ of
    SetDate date next -> do
      H.modify (_ { date = date, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      date <- H.gets _.date
      H.modify (_ { loading = true })
      response <- H.liftAff $ AX.get ("http://localhost:8080/competitions/445/season/2017/" <> date <> "/" <> date)
      log' $ response.response
      H.modify (_ { loading = false, result = Just response.response })
      pure next

log' = H.liftAff <<< log
