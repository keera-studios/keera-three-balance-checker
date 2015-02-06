-- | Updates the balance checker in the model and refresesh it in the view when
-- necessary. Because the balance checker uses a website and is based on webkit
-- (which handles cookies and other goodies for us just beautifully), it runs
-- as a widget in the view.
--
-- (View => Model): Update current balance and expiration dates when new values
-- are available
--
-- (Model => View): Show the new balance and expiration date on the status
-- icon's pixbuf and tooltip
--
-- This situation is suboptimal. Ideally, the update would be performed by a
-- separate, view-less thread that updates the model concurrently, and this
-- module would just present the changes to the user.
module Controller.Conditions.BalanceCheck where

import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.IORef
import Graphics.UI.Gtk

import CombinedEnvironment
import Model.Model (Status(..))
import Hails.MVC.Model.ProtectedModel.Reactive
import Paths
import Three.Balance.Checker as BC

-- | Install bidirectional conditions
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let bc = balanceChecker $ view cenv
      pm = model cenv
   
  -- Update the balance in the view when it changes in the model
  onEvent pm Initialised                $ refreshBalanceMV cenv
  onEvent pm StatusChanged              $ refreshBalanceMV cenv
  onEvent pm LastKnownBalanceChanged    $ refreshBalanceMV cenv
  onEvent pm LastKnownExpirationChanged $ refreshBalanceMV cenv

  -- Update the balance in the model when a new value is obtained from
  -- three.co.uk
  onStatusChanged bc $ refreshBalanceVM cenv

  -- Check the balance every 10 seconds
  timeoutAdd (onViewAsync (checkBalance bc) >> return True) 10000

-- | Updates the status icon reflecting the last known balance
refreshBalanceMV :: CEnv -> IO()
refreshBalanceMV cenv = onViewAsync $ do
  -- View
  icon <- trayIcon $ mainWindowBuilder vw 

  -- Model
  curBalance    <- getter lastKnownBalanceField pm 
  curExpiration <- getter lastKnownExpirationField pm

  -- New tooltip and image
  let tooltip = case (curBalance, curExpiration) of
                 (Just x, Just e)  -> show x ++ " remaining until " ++ e
                 (Just x, Nothing) -> show x ++ " remaining"
                 _                 -> "Could not find balance"
  let imgFn = case curBalance of
               Nothing -> "3mobile-unknown.png"
               Just _  -> "3mobile-sim-card.png"
  getDataFileName imgFn >>= statusIconSetFromFile icon

  -- Update view
  statusIconSetTooltip icon tooltip
  statusIconSetVisible icon True
 where (vw, pm) = (view &&& model) cenv

-- | Updates the model reflecting the last known balance
refreshBalanceVM :: CEnv -> IO()
refreshBalanceVM cenv = onViewAsync $ do

  -- Obtain new values
  st <- readIORef $ BC.status $ balanceChecker vw
  let newBalance    = BC.balance st
      newExpiration = BC.expiration st
      newStatus     = case BC.curStatus st of
                        BC.Loading -> StatusLoading
                        BC.Idle    -> StatusIdle
                        BC.Error   -> StatusError

  -- Update balance and expiration date when available
  when (isJust newBalance) $
    setter lastKnownBalanceField pm newBalance

  when (isJust newExpiration) $ 
    setter lastKnownExpirationField pm newExpiration

  -- Update the status of the icon
  setter statusField pm newStatus
 where (vw, pm) = (view &&& model) cenv
