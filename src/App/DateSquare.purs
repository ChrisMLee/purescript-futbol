module Component.DateSquare where

import Prelude

import CSS as C
import CSS.Flexbox (flexShrink)
import CSS.Overflow (hidden, overflow, overflowY, scroll)
import CSS.Stylesheet (StyleM)
import Data.Bifunctor (bimap)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE

data DateSquareQuery a
  = SelectDate DateTime a

data DateSquareMessage
  = NotifySelect DateTime

-- dateSquareStyle :: StyleM
dateSquareStyle = style do
                    C.padding (C.px 20.0) (C.px 20.0) (C.px 20.0) (C.px 20.0)
                    C.color C.red
                    overflow hidden
                    overflowY scroll
                    flexShrink 0

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
    bimap id id $ HH.li [ dateSquareStyle,
                          HE.onClick (HE.input_ (SelectDate d))
                        ]
                   [ HH.text (either (\err -> "Error parsing date: " <> err) id $ formatDateTime "ddd MMM D" d) ]

  eval :: DateSquareQuery ~> H.ComponentDSL DateTime DateSquareQuery DateSquareMessage m
  eval (SelectDate d next) = do
    H.raise (NotifySelect d)
    pure next
