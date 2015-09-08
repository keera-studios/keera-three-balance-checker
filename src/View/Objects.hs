{-# LANGUAGE TemplateHaskell #-}
module View.Objects where

-- External imports
import Graphics.UI.Gtk
import Hails.MVC.View.Gtk.Builder
import Graphics.UI.Gtk.Extra.BuilderTH

-- Internal imports
import Paths

loadInterface :: IO Builder
loadInterface = loadDefaultInterface getDataFileName

gtkBuilderAccessor "mainMenu"             "Menu"
gtkBuilderAccessor "mainMenuThreeWebsite" "MenuItem"
gtkBuilderAccessor "mainMenuKeeraWebsite" "MenuItem"
gtkBuilderAccessor "mainMenuQuit"         "MenuItem"
gtkBuilderAccessor "trayIcon"             "StatusIcon"
