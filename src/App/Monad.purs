module App.Control.Monad
 (Futbol,
  FutbolM,
  FutbolF (..),
  Environment,
  runFutbol
  ) where

import App.Types (AppEffects, SomeEffects)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Free (Free, liftF, foldFree)
import Control.Monad.Reader (class MonadAsk)
import Halogen.Aff (HalogenEffects)
import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), Unit, discard, flip, id, pure, unit, ($), (<$>), (<<<))
import Control.Monad.Aff (Aff)

type Environment = String

data FutbolF (eff :: # Effect) env a
  = Ask (env -> a)

-- FutbolM is the monad ?
type Futbol = FutbolM (HalogenEffects SomeEffects) Environment

-- TODO: FutbolF is the functor ?
newtype FutbolM eff env a = FutbolM (Free (FutbolF eff env) a)

-- TODO: Why do you need to "un" the monad
unFutbolM :: forall eff env. FutbolM eff env ~> Free (FutbolF eff env)
unFutbolM (FutbolM e) = e

derive newtype instance functorFutbolM :: Functor (FutbolM eff env)
derive newtype instance applyFutbolM :: Apply (FutbolM eff env)
derive newtype instance applicativeFutbolM :: Applicative (FutbolM eff env)
derive newtype instance bindFutbolM :: Bind (FutbolM eff env)
derive newtype instance monadFutbolM :: Monad (FutbolM eff env)

instance monadAskAlerterM :: MonadAsk env (FutbolM eff env) where
  ask = FutbolM <<< liftF <<< Ask $ id

runFutbol :: Environment -> Futbol ~> Aff (HalogenEffects SomeEffects)
runFutbol env = foldFree go <<< unFutbolM

   where

   go :: FutbolF (HalogenEffects SomeEffects) Environment ~> Aff (HalogenEffects SomeEffects)
   go = case _ of
    Ask k ->
      pure (k env)

-- Main difference between two: clvad's uses environment and state


-- type Slam = SlamM SlamDataEffects
--   > SlamDataEffects https://github.com/natefaubion/slamdata/blob/master/src/SlamData/Effects.purs


-- data TerminalF next
--     = GetLine (String -> next)
--     | PrintLine String next
--     deriving Functor

-- type Terminal = Free TerminalF

-- interpret :: Terminal a -> IO a
-- interpret = foldFree morph
--   where
--     morph :: TermF a -> IO a
--     morph (GetLine next) =
--         next <$> getLine
--     morph (PrintLine s n) = do
--         putStrLn s 
--         pure n
