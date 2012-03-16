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

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  let bc = balanceChecker $ view cenv
      pm = model cenv
   
  onEvent pm Initialised                $ refreshBalanceMV cenv
  onEvent pm StatusChanged              $ refreshBalanceMV cenv
  onEvent pm LastKnownBalanceChanged    $ refreshBalanceMV cenv
  onEvent pm LastKnownExpirationChanged $ refreshBalanceMV cenv

  onStatusChanged bc $ refreshBalanceVM cenv

  timeoutAdd (onViewAsync (checkBalance bc) >> return True) 10000

refreshBalanceMV :: CEnv -> IO()
refreshBalanceMV cenv = onViewAsync $ do
  let (vw, pm) = (view &&& model) cenv
  icon <- trayIcon $ mainWindowBuilder vw 

  curBalance    <- getter lastKnownBalanceField pm 
  curExpiration <- getter lastKnownExpirationField pm

  let tooltip = case (curBalance, curExpiration) of
                 (Just x, Just e)  -> show x ++ " remaining until " ++ e
                 (Just x, Nothing) -> show x ++ " remaining"
                 _                 -> "Could not find balance"
  let imgFn = case curBalance of
               Nothing -> "3mobile-unknown.png"
               Just _  -> "3mobile-sim-card.png"

  getDataFileName imgFn >>= statusIconSetFromFile icon
  statusIconSetTooltip icon tooltip
  statusIconSetVisible icon True

refreshBalanceVM :: CEnv -> IO()
refreshBalanceVM cenv = onViewAsync $ do
  let bc = balanceChecker $ view cenv
      pm = model cenv
  st <- readIORef (BC.status bc)
      
  let newBalance    = BC.balance st
      newExpiration = BC.expiration st
      newStatus     = case BC.curStatus st of
                        BC.Loading -> StatusLoading
                        BC.Idle    -> StatusIdle
                        BC.Error   -> StatusError

  when (isJust newBalance) $
    setter lastKnownBalanceField pm newBalance

  when (isJust newExpiration) $ 
    setter lastKnownExpirationField pm newExpiration

  setter statusField pm newStatus
