{-# LANGUAGE PatternGuards #-}
module Three.Balance.Checker where

import Control.Monad
import Data.IORef
import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebDataSource
import Graphics.UI.Gtk.WebKit.WebFrame
-- import Graphics.UI.Gtk.WebKit.Download
import Three.Balance.TagSoup
import Text.HTML.TagSoup

data BalanceChecker = BalanceChecker
   { status  :: IORef Status
   , conf    :: IORef Int
   , webView :: WebView
   , hndlrs  :: IORef [ IO () ]
   }

data Status = Status
   { balance    :: Maybe Int
   , expiration :: Maybe String
   , curStatus  :: WebkitStatus
   }
 deriving Show

data WebkitStatus = Loading
                  | Idle
                  | Error
 deriving Show

checkBalance :: BalanceChecker -> IO()
checkBalance bc = webViewLoadUri (webView bc) balanceURL

balanceURL :: String
balanceURL = "https://www.three.co.uk/My3Account/MBB_PAYG/Allowance"

onStatusChanged :: BalanceChecker -> IO () -> IO ()
onStatusChanged bc hndl = do
   hndls <- readIORef (hndlrs bc)
   let hndls' = hndls ++ [hndl]
   writeIORef (hndlrs bc) hndls'
  
balanceCheckerNew :: IO BalanceChecker
balanceCheckerNew = do

  -- Create WebKit view.
  webView <- webViewNew

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

triggerStatusHandlers :: BalanceChecker -> IO()
triggerStatusHandlers bc =
  mapM_ id =<< readIORef (hndlrs bc)

threeBalanceLoadStarted :: BalanceChecker -> IO()
threeBalanceLoadStarted bc = do
    st <- readIORef (status bc)
    let st' = st { curStatus = Loading }
    writeIORef (status bc) st'
    triggerStatusHandlers bc

threeBalanceLoadIdle :: BalanceChecker -> IO()
threeBalanceLoadIdle bc = do
    st <- readIORef (status bc)
    let st' = st { curStatus = Idle }
    writeIORef (status bc) st'
    triggerStatusHandlers bc

reloadTillNecessary :: WebView -> String -> IO Bool
reloadTillNecessary webView url = do
  uri <- webViewGetUri webView
  when (uri /= Just url) $ webViewLoadUri webView url
  return (uri == Just url)
  
accessBalance :: WebFrame -> BalanceChecker -> IO()
accessBalance webFrame bc = do
  contents <- webDataSourceGetData =<< webFrameGetDataSource webFrame
  case contents of
    Just str -> do st <- readIORef (status bc)
                   let tags = parseTags str
                       st'  = newStatus st tags
                   writeIORef (status bc) st'
    Nothing  -> return ()
  
newStatus :: Status -> [Tag String] -> Status
newStatus st ts
  | Just (exp, n) <- findBroadbandData ts
  = st { balance = Just n, expiration = Just exp }
  | Just n <- findTotalBroadbandData ts
  = st { balance = Just n, expiration = Nothing }
  | otherwise
  = st { balance = Nothing, expiration = Nothing }
