module Main where

import Prelude

import App.Control.Monad (Futbol, runFutbol)
import App.Types (AppEffects, SomeEffects)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Data.JSDate (LOCALE)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import ParentComponent (ui)

-- type AppEffects eff =
--   ( console :: CONSOLE
--   , dom :: DOM
--   , ajax :: AX.AJAX
--   | eff)

-- | Run the app.
main :: Eff (HA.HalogenEffects SomeEffects) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let ui' = H.hoist (runFutbol "HI") ui
  runUI ui' unit =<< HA.awaitBody
  -- runUI ui unit body
