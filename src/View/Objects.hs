{-# LANGUAGE TemplateHaskell #-}
module View.Objects where

-- External imports
import Graphics.UI.Gtk
import Hails.Graphics.UI.Gtk.Builder
import Hails.Graphics.UI.Gtk.THBuilderAccessor

-- Internal imports
import Paths

loadInterface :: IO Builder
loadInterface = loadDefaultInterface getDataFileName

gtkBuilderAccessor "mainMenu"             "Menu"
gtkBuilderAccessor "mainMenuThreeWebsite" "MenuItem"
gtkBuilderAccessor "mainMenuKeeraWebsite" "MenuItem"
gtkBuilderAccessor "mainMenuQuit"         "MenuItem"
gtkBuilderAccessor "trayIcon"             "StatusIcon"
