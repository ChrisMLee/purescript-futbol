module DateSection where

import App.Lenses
import App.Types
import CSS.Display
import Data.Time
import Prelude

import Component.DateSquare (DateSquareQuery(..), DateSquareMessage(..), dateSquare)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Array (cons, nub)
import Data.DateTime (DateTime(..), modifyTime, setHour, setMillisecond, setMinute, setSecond)
import Data.Enum (toEnum)
import Data.Foldable (foldr)
import Data.JSDate as JSD
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style)
import Halogen.HTML.Events as HE
import Partial.Unsafe (unsafePartial)

-- this is how you prevent duplicate slot addresses
newtype DateSquareSlot = DateSquareSlot DateTime
derive instance eqDateSquareSlot :: Eq DateSquareSlot
derive instance ordDateSquareSlot :: Ord DateSquareSlot

type Input = Fixtures

type State = Array DateTime


initialState :: State
initialState = []

data DateSectionQuery a
  =
    HandleInput Fixtures a
  | HandleDateSquareMessage DateTime DateSquareMessage a

data DateSectionMessage =
  NotifyDateSelect DateTime

zeroOutTime :: Time -> Time
zeroOutTime = setHour h <<< setMinute m <<< setSecond s <<< setMillisecond ms where
                  h = unsafePartial $ fromJust $ toEnum 0
                  m = unsafePartial $ fromJust $ toEnum 0
                  s = unsafePartial $ fromJust $ toEnum 0
                  ms = unsafePartial $ fromJust $ toEnum 0

makeDateTime :: forall eff. String -> (Aff (AppEffects eff)) DateTime
makeDateTime s = do
  parsed <- liftEff $ JSD.parse s
  pure $ unsafePartial $ fromJust $ JSD.toDateTime parsed

fixtureDates :: Fixtures -> Array String
fixtureDates fixtures = foldr grabDate [] fixtures where
                          grabDate (Fixture f) acc = cons f.date acc

dateSection :: forall eff. H.Component HH.HTML DateSectionQuery Input DateSectionMessage (Aff (AppEffects eff))
dateSection =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ParentHTML DateSectionQuery DateSquareQuery DateSquareSlot (Aff (AppEffects eff))
  render state =
    HH.div_
      [ HH.ul [style $ do display flex] (map renderDateSquare state) ]

  renderDateSquare :: DateTime -> H.ParentHTML DateSectionQuery DateSquareQuery DateSquareSlot (Aff (AppEffects eff))
  renderDateSquare dateTime =
    HH.slot
      (DateSquareSlot dateTime)
      (dateSquare dateTime)
      unit
      (HE.input (HandleDateSquareMessage dateTime))

  eval :: DateSectionQuery ~> H.ParentDSL State DateSectionQuery DateSquareQuery DateSquareSlot DateSectionMessage (Aff (AppEffects eff))
  eval = case _ of
    HandleInput f next -> do
      H.liftAff $ log $ show $ nub $ fixtureDates f
      fd <- H.liftAff $ (sequence $ map makeDateTime $ (fixtureDates f))
      ud <- pure $ nub $ (map (modifyTime zeroOutTime) fd)
      H.liftAff $ log $ show $ ud
      H.put ud
      pure next
    HandleDateSquareMessage p msg next -> do
      case msg of
        NotifySelect d -> do
          -- H.liftAff $ log $ show $ d
          -- wasComplete <- H.query (TaskSlot p) (H.request IsCompleted)
          -- when (fromMaybe false wasComplete) $ H.modify $ updateNumCompleted (_ `sub` 1)
          -- H.modify (removeTask p)
          H.raise (NotifyDateSelect d)
      pure next


