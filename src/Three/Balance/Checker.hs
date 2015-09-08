{-# LANGUAGE PatternGuards #-}
-- | Checks the status of your Three balance
--
-- The BalanceChecker encapsulates a Gtk widget and checks your PayG allowance.
--
-- It includes methods to request a balance refresh and detect when a new value
-- has been obtained.
module Three.Balance.Checker where

import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
import Graphics.UI.Gtk.WebKit.CacheModel
import Text.HTML.TagSoup

import Three.Balance.TagSoup

-- | Encapsulates a webkit widget, the status and the listeners to execute on
-- status changes
data BalanceChecker = BalanceChecker
   { status  :: IORef Status
   , conf    :: IORef Int
   , webView :: WebView
   , hndlrs  :: IORef [ IO () ]
   }

-- | Summarizes the status of your PayG balance
data Status = Status
   { balance    :: Maybe Int
   , expiration :: Maybe String
   , curStatus  :: WebkitStatus
   }
 deriving Show

-- | The three possible states in which the widget can be at each time
data WebkitStatus = Loading
                  | Idle
                  | Error
 deriving Show

checkBalance :: BalanceChecker -> IO()
checkBalance bc = webViewLoadUri (webView bc) balanceURL

balanceURL :: String
balanceURL = "https://www.three.co.uk/My3Account/MBB_PAYG/Allowance"

-- | Installs a handler to be execute when the status changes
onStatusChanged :: BalanceChecker -> IO () -> IO ()
onStatusChanged bc hndl = do
   hndls <- readIORef (hndlrs bc)
   let hndls' = hndls ++ [hndl]
   writeIORef (hndlrs bc) hndls'
  
-- | Creates a new Balance Checker
balanceCheckerNew :: IO BalanceChecker
balanceCheckerNew = do

  -- Create WebKit view.
  webView <- webViewNew

  setCacheModel CacheModelDocumentBrowser

  let url = balanceURL
      wb  = webView

  -- Load uri.
  -- webViewLoadUri webView url

  st <- newIORef Status { balance    = Nothing
                        , expiration = Nothing
                        , curStatus  = Idle
                        }

  cf <- newIORef (60 :: Int)
  hndls <- newIORef []

  let bc = BalanceChecker
           { status  = st
           , conf    = cf
           , webView = wb
           , hndlrs  = hndls
           }

  -- Open all link in current window.
  webView `on` createWebView $ \frame -> do
    newUri <- webFrameGetUri frame
    case newUri of
      Just uri -> webViewLoadUri webView uri
      Nothing  -> return ()
    return webView

  -- webView `on` downloadRequested $ \download -> do
  --   newUri <- downloadGetDestinationUri download
  --   putStrLn $ show newUri
  --   return False

  webView `on` documentLoadFinished $ \frame -> do
    loaded <- reloadTillNecessary webView url
    if loaded
     then threeBalanceLoadIdle bc >> accessBalance frame bc
     else threeBalanceLoadStarted bc

  webView `on` loadStarted $ const $ threeBalanceLoadStarted bc

  return bc

-- | Executes the existing status handlers in sequence
triggerStatusHandlers :: BalanceChecker -> IO()
triggerStatusHandlers bc = sequence_ =<< readIORef (hndlrs bc)

-- | Sets the widget status as loading
threeBalanceLoadStarted :: BalanceChecker -> IO()
threeBalanceLoadStarted bc = do
    st <- readIORef (status bc)
    let st' = st { curStatus = Loading }
    writeIORef (status bc) st'
    triggerStatusHandlers bc

-- | Sets the widget status as idle
threeBalanceLoadIdle :: BalanceChecker -> IO()
threeBalanceLoadIdle bc = do
    st <- readIORef (status bc)
    let st' = st { curStatus = Idle }
    writeIORef (status bc) st'
    triggerStatusHandlers bc

--- | Tries to load a page if it's not the current one
reloadTillNecessary :: WebView -> String -> IO Bool
reloadTillNecessary webView url = do
  uri <- webViewGetUri webView
  when (uri /= Just url) $ webViewLoadUri webView url
  return (uri == Just url)
  
-- | Obtains and updates the current balance in the balance checker
accessBalance :: WebFrame -> BalanceChecker -> IO()
accessBalance webFrame bc = do
  contents <- webDataSourceGetData =<< webFrameGetDataSource webFrame
  case contents of
    Just str -> do st <- readIORef (status bc)
                   let tags = parseTags str
                       st'  = newStatus st tags
                   writeIORef (status bc) st'
    Nothing  -> return ()
  
-- | Calculates the new status from the contents of the webpage provided by
-- Three.co.uk when requesting the remaining allowance.
newStatus :: Status -> [Tag String] -> Status
newStatus st ts
  | Just (exp, n) <- findBroadbandData ts
  = st { balance = Just n, expiration = Just exp }
  | Just n <- findTotalBroadbandData ts
  = st { balance = Just n, expiration = Nothing }
  | otherwise
  = st { balance = Nothing, expiration = Nothing }
