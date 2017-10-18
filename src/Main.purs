module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax as AX
import ParentComponent (ui)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Control.Monad.Eff.Now (NOW)

type AppEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  , ajax :: AX.AJAX
  | eff)

-- | Run the app.
main ::  forall e. Eff (AppEffects ( avar :: AVAR, ref :: REF, exception :: EXCEPTION, now :: NOW| e)
) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
