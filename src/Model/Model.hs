module Model.Model where

import Data.ExtraVersion
import Hails.MVC.Model.ProtectedModel.VersionedModel
import Hails.MVC.Model.ProtectedModel.UpdatableModel

data Model = Model
 {

 -- Current status
   status              :: Status

 -- Last known balance
 , lastKnownBalance    :: Maybe Int
 , lastKnownExpiration :: Maybe String

 -- Updating system 
 , programName         :: String
 , programVersion      :: Version
 , programMaxVersion   :: Maybe Version
 , programUpdateURI    :: String

 }
 deriving (Eq)

data Status = StatusIdle
            | StatusLoading
            | StatusError
 deriving (Eq, Ord)

emptyBM :: Model
emptyBM = Model
  { lastKnownBalance     = Nothing
  , lastKnownExpiration  = Nothing
 
  -- Current status
  , status              = StatusIdle
 
  -- Update check
  , programName         = "Keera Three PayG Balance Checker"
  , programVersion      = Version 0 0 Beta 0
  , programMaxVersion   = Nothing
  , programUpdateURI    = "http://www.keera.es/projects/keera-payg/newest-version"
  }

instance VersionedBasicModel Model where
  getBMVersion = programVersion

instance UpdatableBasicModel Model where
  getBMUpdateURI            = programUpdateURI
  getBMMaxVersionAvail      = programMaxVersion
  setBMMaxVersionAvail bm x = bm { programMaxVersion = Just x }
