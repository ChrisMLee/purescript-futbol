module Component.DateSquare where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Bifunctor (bimap)

data DateQuery a
  = SelectDate a

data DateMessage
  = NotifySelect

dateSquare :: forall m. DateTime -> H.Component HH.HTML DateQuery Unit DateMessage m
dateSquare initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: DateTime -> H.ComponentHTML DateQuery
  render d =
    bimap id id $ HH.h1_ [ HH.text "Standin Date" ]

  eval :: DateQuery ~> H.ComponentDSL DateTime DateQuery DateMessage m
  eval (SelectDate next) = do
    H.raise NotifySelect
    pure next
