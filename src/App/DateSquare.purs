module Component.DateSquare where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..))
import Data.Bifunctor (bimap)
import Data.Formatter.DateTime (formatDateTime)
import Data.Either (either)

data DateSquareQuery a
  = SelectDate a

data DateSquareMessage
  = NotifySelect

dateSquare :: forall m. DateTime -> H.Component HH.HTML DateSquareQuery Unit DateSquareMessage m
dateSquare initialState =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: DateTime -> H.ComponentHTML DateSquareQuery
  render d =
    bimap id id $ HH.li_ [ HH.text (either (\err -> "Error parsing date: " <> err) id $ formatDateTime "ddd MMM D" d) ]

  eval :: DateSquareQuery ~> H.ComponentDSL DateTime DateSquareQuery DateSquareMessage m
  eval (SelectDate next) = do
    H.raise NotifySelect
    pure next
