-- | Contains basic operations related to the GUI
module View
  ( module View
  , module Exported
  )
  where

-- External libraries
import Graphics.UI.Gtk
import Graphics.UI.Gtk.GtkView
import Hails.MVC.View.GladeView
import Hails.MVC.View.GtkView as Exported

-- Internal libraries
import View.MainWindow.Objects
import Three.Balance.Checker

instance GtkGUI View where
  initialise = createView

instance GladeView View where
  ui = mainWindowBuilder

-- | This datatype should hold the elements that we must track in the future
-- (for instance, treeview models)
data View = View
  { mainWindowBuilder :: Builder
  , balanceChecker    :: BalanceChecker

  }

createView :: IO View
createView = do
  bldr  <- loadInterface
  bc    <- balanceCheckerNew

  return
    View
      { mainWindowBuilder = bldr
      , balanceChecker    = bc
      }

