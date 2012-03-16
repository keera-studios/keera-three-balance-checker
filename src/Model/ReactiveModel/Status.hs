{-# LANGUAGE TemplateHaskell #-}
module Model.ReactiveModel.Status where

-- External imports
import Data.Maybe
import qualified Hails.MVC.Model.ReactiveFields as RFs
import Hails.MVC.Model.ReactiveFields 
         (fieldGetter, fieldSetter, preTrue)
import Hails.MVC.Model.THFields

-- Internal imports
import Model.Model
import Model.ReactiveModel.ReactiveModelInternals
import Model.ReactiveModel.ModelEvents

-- A Field of type A lets us access a reactive field of type a from
-- a Model, and it triggers a ModelEvent
type Field a = RFs.Field a Model ModelEvent

-- | Set the new status.
--
-- The status remains unmodified if a model invariant is violated.
setStatus :: ReactiveModel -> Status -> ReactiveModel
setStatus rm n
 -- Nothing has changed
 | getStatus rm == n = rm

 -- Ok
 | otherwise   = triggerEvent rm' ev
  where rm'    = rm `onBasicModel` (\b -> b { status = n })
        ev     = StatusChanged

getStatus :: ReactiveModel -> Status
getStatus = status . basicModel

-- | Basic settings
reactiveField "LastKnownBalance"    [t|Maybe Int|]
reactiveField "LastKnownExpiration" [t|Maybe String|]
