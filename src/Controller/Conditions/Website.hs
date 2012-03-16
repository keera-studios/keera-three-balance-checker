-- | Opens the default browser with the project URL
module Controller.Conditions.Website where

import Control.Monad
import Graphics.UI.Gtk
import Hails.Graphics.UI.Gtk.Helpers.MessageDialog

import CombinedEnvironment
import System.Application

installHandlers :: CEnv -> IO()
installHandlers cenv = do
  associateMenuURL cenv mainMenuKeeraWebsite 
                          "http://www.keera.es/projects/keera-posture/"
  associateMenuURL cenv mainMenuThreeWebsite
                          "https://www.three.co.uk/My3Account/MBB_PAYG/Allowance"

associateMenuURL :: CEnv -> (Builder -> IO MenuItem) -> String -> IO()
associateMenuURL cenv f url = void $ do
  menu <- f $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $
    onViewAsync $
       openUrlBySystemTool url >>= (`unless` reportNoBrowser)

reportNoBrowser :: IO ()
reportNoBrowser =
  popupError "Keera PayG Balance Checker" 
             "The URL cannot be opened because there's no program associated to text/html"
