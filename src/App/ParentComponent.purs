module ParentComponent where

import App.Lenses
import App.Types
import Optic.Core
import Prelude

import App.Helpers (fixtureDates, makeDateTime, zeroOutTime)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Now (now, NOW)
import DOM (DOM)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, head, length, nub, reverse, snoc, sortBy)
import Data.Combinators (on)
import Data.DateTime (DateTime(..), diff, modifyTime)
import Data.DateTime.Instant as DTI
import Data.Either (Either(Right, Left), either)
import Data.Foldable (foldr)
import Data.Formatter.DateTime as DFDT
import Data.Int (round)
import Data.JSDate as JSD
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (over)
import Data.Time.Duration (class Duration, Milliseconds(..), fromDuration)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd, fst)
import DateSection (DateSectionMessage(..), DateSectionQuery(..), dateSection)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Math (abs)
import Network.HTTP.Affjax as AX
import Partial.Unsafe (unsafePartial)

data Slot = DateSectionSlot
derive instance eqDateSectionSlot :: Eq Slot
derive instance ordDateSectionSlot :: Ord Slot

type State =
  { loading :: Boolean
  , date :: Maybe DateTime
  , selectedDate :: Maybe DateTime
  , result :: Fixtures
  }

initialState :: State
initialState = { loading: false, selectedDate: Nothing, date: Nothing, result: []}

data Query a
  = Initialize a
  | Finalize a
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
      , HH.div_
          case state.date of
            Nothing -> [HH.text  "No date loaded"]
            (Just d) -> [HH.text ("date: " <> (either (\err -> "Error parsing date: " <> err) id $ DFDT.formatDateTime "ddd MMM D" d))]
      , HH.div_
          case state.selectedDate of
            Nothing -> [HH.text "No date selected"]
            (Just d) -> [HH.text ("selected date: " <>(either (\err -> "Error parsing date: " <> err) id $ DFDT.formatDateTime "ddd MMM D" d))]
      , HH.text (if state.loading then "Working..." else "")
      , HH.slot (DateSectionSlot) dateSection (state.result) (HE.input HandleDateSectionMessage)
      , HH.div_
          case length $ state.result of
          0 ->
            [ HH.div_ [HH.text "No Fixtures"]]
          _ ->
            [HH.ul_ (map fixtureComponent state.result)]
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
      -- H.liftAff $ log response.response
      let receiveFixtures (Right x) = do
            filteredDates <- H.liftAff $ (sequence $ map makeDateTime $ (fixtureDates x))
            uniqueDates <- pure $ nub $ (map (modifyTime zeroOutTime) filteredDates)
            cd <- pure $ closestDate currentTime uniqueDates
            H.liftAff $ log $ show $ closestDate currentTime uniqueDates
            H.modify (_ { loading = false, result = x, selectedDate = Just currentTime })
          receiveFixtures (Left err) = do
            H.liftAff $ log err
            H.modify (_ { loading = false, result = [] })
      fixtures <- pure $ jsonParser response.response >>= decodeJson
      receiveFixtures $ fixtures
      pure next
    Finalize next -> do
      pure next
    HandleDateSectionMessage msg next -> do
      case msg of
        NotifyDateSelect d -> do
          H.liftAff $ log $ show $ d
          H.modify (_ { selectedDate = Just d })
      pure next

getScore :: Maybe Number -> String
getScore Nothing = "Future"
getScore (Just x) = show $ round $ x

fixtureComponent :: forall s p i. Fixture -> H.HTML p i
fixtureComponent (Fixture f) = let
                                 hometeam  = f.homeTeamName
                                 homegoals = getScore $ f ^. result.._Result..goalsHomeTeam
                                 awayteam  = f.awayTeamName
                                 awaygoals = getScore $ f ^. result.._Result..goalsAwayTeam
                               in
                               HH.li_ [
                                         HH.div_ [ HH.text hometeam
                                                 , HH.text homegoals
                                                 ]
                                       , HH.div_ [ HH.text awayteam
                                                 , HH.text awaygoals
                                                 ]
                                       ]

closestDate :: DateTime -> Array DateTime -> Maybe DiffDate
closestDate givenDate [] = Nothing
closestDate givenDate xs = foldr returnClosest Nothing s where
                           s :: Array DiffDate
                           s = mySort $ (map (\x -> getDiffDate givenDate x) xs)
                           returnClosest :: DiffDate -> Maybe DiffDate -> Maybe DiffDate
                           returnClosest d Nothing = Just d
                           returnClosest d acc = if (absMilliseconds $ fst d) < (absMilliseconds $ fst (unsafePartial $ fromJust acc)) then Just d else acc

-- mySort :: forall b. Ord b => [(a, b)] -> [(a, b)]
mySort = sortBy (flip compare `on` fst)

absMilliseconds :: Milliseconds -> Milliseconds
absMilliseconds = over Milliseconds abs

  -- do
  --                          -- coolest <- pure givenDate
  --                          cool <- pure $ diff givenDate (fromMaybe givenDate (unsafePartial head xs)) :: Milliseconds
  --                          -- log $ show cool
  --                          (unsafePartial head xs)

type DiffDate = Tuple Milliseconds DateTime

getDiffDate :: DateTime -> DateTime -> DiffDate
getDiffDate d d' = Tuple (diff d d') (d')

-- [(diff, dateTime)]

-- (diff givenDate (unsafePartial head xs))

-- diff :: forall d. Duration d => DateTime -> DateTime -> d
-- will take todays date and find the available date that most closely matches it
-- closestDate :: DateTime -> Array DateTime -> Maybe DateTime
-- closestDate givenDate [] = Nothing
-- closestDate givenDate xs = foldr returnClosest Nothing xs where
--                      -- isCloser :: Duration Milliseconds -> Duration Milliseconds -> Boolean
--                      isCloser x y = x
--                      returnClosest :: DateTime -> Maybe DateTime -> Maybe DateTime
--                      returnClosest d Nothing = Just d
--                      returnClosest d (Just acc) = if (isCloser (diff givenDate d) (diff givenDate acc)) then (Just d) else (Just acc)


-- if diff iteratedtime and currentdate is less than diff current acc, then it becomes the acc

      -- fd <- H.liftAff $ (sequence $ map makeDateTime $ (fixtureDates f))
      -- ud <- pure $ nub $ (map (modifyTime zeroOutTime) fd)

-- can you use do syntax in render method?
