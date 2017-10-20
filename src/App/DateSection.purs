module DateSection where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..), fromJust)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Array (cons, nub)
import Data.Foldable (foldr)
import App.Types
import App.Lenses
import Data.DateTime (DateTime)
import Data.JSDate as JSD
import Partial.Unsafe (unsafePartial)

type Input = Fixtures

type State = Fixtures

initialState :: State
initialState = []

data Query a = HandleInput Fixtures a

type DateSectionEff eff = Aff (console :: CONSOLE) eff

makeDateTime :: forall eff. String -> (Aff (AppEffects eff)) DateTime
makeDateTime s = do
  parsed <- liftEff $ JSD.parse s
  pure $ unsafePartial $ fromJust $ JSD.toDateTime parsed

fixtureDates :: Fixtures -> Array String
fixtureDates fixtures = foldr grabDate [] fixtures where
                          grabDate (Fixture f) acc = nub $ cons f.date acc

component :: forall eff. H.Component HH.HTML Query Input Void (Aff (AppEffects eff))
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
      , HH.strong_ [ HH.text (show $ fixtureDates state) ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (AppEffects eff))
  eval = case _ of
    HandleInput f next -> do
      H.put f
      pure next

