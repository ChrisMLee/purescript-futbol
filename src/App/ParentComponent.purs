module ParentComponent where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Array (snoc, filter, reverse)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import App.Types
import DateSection as DateSection

data Slot = DateSectionSlot
derive instance eqDateSectionSlot :: Eq Slot
derive instance ordDateSectionSlot :: Ord Slot

type State =
  { loading :: Boolean
  , date :: String
  , result :: Fixtures
  , fakeCount :: Int
  }

initialState :: State
initialState = { loading: false, date: "", result: [], fakeCount: 0 }

data Query a
  = Initialize a
  | Finalize a
  | Increment a

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
ui = H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

  render :: State -> H.ParentHTML Query DateSection.Query Slot (Aff (AppEffects eff))
  render state =
    HH.div_
      [
        HH.h1_ [ HH.text "YOWZERS" ]
      , HH.slot (DateSectionSlot) DateSection.component (state.fakeCount) absurd
      , HH.button
        [ HE.onClick (HE.input_ Increment) ]
        [ HH.text "+1"]
      ]

  eval :: Query ~> H.ParentDSL State Query DateSection.Query Slot Void (Aff (AppEffects eff))
  eval (Initialize next) = do
    H.liftAff $ log "Initialize Root"
    pure next
  eval (Finalize next) = do
    pure next
  eval (Increment next) = do
    H.modify (\st -> st { fakeCount = st.fakeCount + 1 })
    pure next


