-- {-# LANGUAGE CPP #-}
-- | Shows the popup menu when the user right-clicks the icon
module Controller.Conditions.PopupMenu where

import Control.Monad
import Graphics.UI.Gtk

import CombinedEnvironment

installHandlers :: CEnv -> IO()
installHandlers cenv = void $ do
  icon <- trayIcon $ mainWindowBuilder $ view cenv
  icon `on` statusIconPopupMenu $ condition cenv

condition :: CEnv -> Maybe MouseButton -> TimeStamp -> IO()
condition cenv m t = onViewAsync $ do
  menu <- mainMenu $ mainWindowBuilder $ view cenv
  -- This should be some other way

  let m' = fmap (flip (,) t) m
  menuPopup menu m'
