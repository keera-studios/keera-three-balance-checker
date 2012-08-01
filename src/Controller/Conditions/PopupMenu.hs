-- | Shows the popup menu when the user right-clicks the icon
--
-- (View => View)
module Controller.Conditions.PopupMenu where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment

-- | Installs the event listeners
installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  icon `on` statusIconPopupMenu $ condition cenv

-- | Shows the menu when the user clicks on the icon
condition :: CEnv -> Maybe MouseButton -> TimeStamp -> IO()
condition cenv m t = onViewAsync $ do
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  let m' = fmap (flip (,) t) m
  menuPopup menu m'
