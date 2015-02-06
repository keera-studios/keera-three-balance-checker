-- | Opens the default browser with the project URL
module Controller.Conditions.Website where

import Control.Monad
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Helpers.MessageDialog

import CombinedEnvironment
import System.Application

-- | Installs the event handlers for the website and Three's Allowance checking
-- website
installHandlers :: CEnv -> IO()
installHandlers cenv = do
  associateMenuURL cenv mainMenuKeeraWebsite 
                          "https://github.com/ivanperez-keera/keera-three-balance-checker"
  associateMenuURL cenv mainMenuThreeWebsite
                          "https://www.three.co.uk/My3Account/MBB_PAYG/Allowance"

-- | Installs an event handler on a menu item that opens a website when
-- activated
associateMenuURL :: CEnv -> (Builder -> IO MenuItem) -> String -> IO()
associateMenuURL cenv f url = void $ do
  menu <- f $ mainWindowBuilder $ view cenv
  menu `on` menuItemActivate $
    onViewAsync $
       openUrlBySystemTool url >>= (`unless` reportNoBrowser)

-- | Reports that no browser has been found
reportNoBrowser :: IO ()
reportNoBrowser =
  popupError "Keera PayG Balance Checker" 
             "The URL cannot be opened because there's no program associated to text/html"
