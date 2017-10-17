module DateSection where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import App.Types

type Input = Fixtures

type State = Fixtures

initialState :: State
initialState = []

data Query a = HandleInput Fixtures a

type DateSectionEff eff = Aff (console :: CONSOLE) eff

component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.text "My input value is:"
      , HH.strong_ [ HH.text (show state) ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    HandleInput f next -> do
      H.put f
      pure next

