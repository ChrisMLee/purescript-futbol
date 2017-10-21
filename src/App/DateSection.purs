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
import Data.Traversable (sequence)
import Component.DateSquare (DateSquareQuery(..), DateSquareMessage(..), dateSquare)

data DateSquareSlot = DateSquareSlot
derive instance eqDateSquareSlot :: Eq DateSquareSlot
derive instance ordDateSquareSlot :: Ord DateSquareSlot

type Input = Fixtures

type State = Array DateTime


initialState :: State
initialState = []

data Query a
  =
    HandleInput Fixtures a
  | HandleDateSquareMessage DateTime DateSquareMessage a

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
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ParentHTML Query DateSquareQuery DateSquareSlot (Aff (AppEffects eff))
  render state =
    HH.div_
      [ HH.text "My input value is:"
      , HH.ul_ (map renderDateSquare state)
      ]

  renderDateSquare :: DateTime -> H.ParentHTML Query DateSquareQuery DateSquareSlot (Aff (AppEffects eff))
  renderDateSquare dateTime =
    HH.slot
      DateSquareSlot
      (dateSquare dateTime)
      unit
      (HE.input (HandleDateSquareMessage dateTime))

  eval :: Query ~> H.ParentDSL State Query DateSquareQuery DateSquareSlot Void (Aff (AppEffects eff))
  eval = case _ of
    HandleInput f next -> do
      fd <- H.liftAff $ sequence $ map makeDateTime $ (fixtureDates f)
      H.liftAff $ log $ show fd
      H.put fd
      pure next
    HandleDateSquareMessage p msg next -> do
      case msg of
        NotifySelect -> do
          H.liftAff $ log $ "hi"
          -- wasComplete <- H.query (TaskSlot p) (H.request IsCompleted)
          -- when (fromMaybe false wasComplete) $ H.modify $ updateNumCompleted (_ `sub` 1)
          -- H.modify (removeTask p)
      pure next


