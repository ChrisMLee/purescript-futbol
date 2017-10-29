module ParentComponent where

import App.Types
import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (now, NOW)
import DOM (DOM)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (snoc, filter, reverse)
import Data.DateTime (DateTime)
import Data.DateTime.Instant as DTI
import Data.Either (Either(Right, Left), either)
import Data.Formatter.DateTime (formatDateTime)
import Data.JSDate as JSD
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DateSection (DateSectionMessage(..), DateSectionQuery(..), dateSection)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Network.HTTP.Affjax as AX

data Slot = DateSectionSlot
derive instance eqDateSectionSlot :: Eq Slot
derive instance ordDateSectionSlot :: Ord Slot

type State =
  { loading :: Boolean
  , date :: Maybe DateTime
  , result :: Fixtures
  , fakeCount :: Int
  }

initialState :: State
initialState = { loading: false, date: Nothing, result: [], fakeCount: 0 }

data Query a
  = Initialize a
  | Finalize a
  | Increment a
  | HandleDateSectionMessage DateSectionMessage a


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

  render :: State -> H.ParentHTML Query DateSectionQuery Slot (Aff (AppEffects eff))
  render state =
    HH.div_
      [
        HH.h1_ [ HH.text "Fixtures" ]
      , HH.text case state.date of
                  Nothing -> "No date loaded"
                  (Just d) -> (either (\err -> "Error parsing date: " <> err) id $ formatDateTime "ddd MMM D" d)
      , HH.text (if state.loading then "Working..." else "")
      , HH.slot (DateSectionSlot) dateSection (state.result) (HE.input HandleDateSectionMessage)
      , HH.button
        [ HE.onClick (HE.input_ Increment) ]
        [ HH.text "+1"]
      ]

  eval :: Query ~> H.ParentDSL State Query DateSectionQuery Slot Void (Aff (AppEffects eff))
  eval = case _ of
    Initialize next -> do
      H.liftAff $ log "Initialize Root"
      -- TODO: use state monad to pass around configuration
      H.modify (_ { loading = true })
      currentTime <- DTI.toDateTime <$> (H.liftEff now)
      H.modify (_ {date = Just currentTime})
      H.liftAff $ log $ show currentTime
      -- testTime <- H.liftEff $ JSD.parse  "2017-08-12T14:00:00Z"
      -- H.liftAff $ log $ show $ JSD.toDateTime testTime
      response <- H.liftAff $ AX.get ("http://localhost:8080/competitions/445/fixtures")
      H.liftAff $ log response.response
      let receiveFixtures (Right x) = H.modify (_ { loading = false, result = x })
          receiveFixtures (Left err) = do
            H.liftAff $ log err
            H.modify (_ { loading = false, result = [] })
      fixtures <- pure $ jsonParser response.response >>= decodeJson
      receiveFixtures $ fixtures
      pure next
    Finalize next -> do
      pure next
    Increment next -> do
      H.modify (\st -> st { fakeCount = st.fakeCount + 1 })
      pure next
    HandleDateSectionMessage msg next -> do
      case msg of
        NotifyDateSelect d -> do
          H.liftAff $ log $ show $ d
          H.modify (_ { date = Just d })
      pure next



