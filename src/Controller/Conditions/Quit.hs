-- | Exits the application when requested by the user
--
-- (View => IO)
module Controller.Conditions.Quit where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  menu <- mainMenuQuit $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $ condition cenv

condition :: CEnv -> IO()
condition cenv = do
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  statusIconSetVisible icon False
  mainQuit
