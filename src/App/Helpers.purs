module App.Helpers where

import App.Types

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, nub)
import Data.DateTime (DateTime(..), modifyTime, setHour, setMillisecond, setMinute, setSecond)
import Data.Enum (toEnum)
import Data.Foldable (foldr)
import Data.JSDate as JSD
import Data.Maybe (Maybe(..), fromJust)
import Data.Time (Time, setHour, setMillisecond, setMinute, setSecond)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, pure, ($), (<<<))

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

fixtureDates :: Fixtures -> Array DateTime
fixtureDates fixtures = foldr grabDate [] fixtures where
                          grabDate (Fixture f) acc = cons f.date acc
