module Model.ReactiveModel.ModelEvents
 ( ModelEvent ( StatusChanged 
              , LastKnownBalanceChanged
              , LastKnownExpirationChanged
              , Initialised
              , MaxVersionAvailable
              )
 ) where

import qualified Hails.MVC.Model.ReactiveModel as GRM
import Hails.MVC.Model.ReactiveModel.Events
import Hails.MVC.Model.ProtectedModel.UpdatableModel

data ModelEvent = UncapturedEvent
                | StatusChanged
                | LastKnownBalanceChanged
                | LastKnownExpirationChanged
                | Initialised
                | MaxVersionAvailable
 deriving (Eq,Ord)

instance GRM.Event ModelEvent where
  undoStackChangedEvent = UncapturedEvent

instance UpdateNotifiableEvent ModelEvent where
  updateNotificationEvent = MaxVersionAvailable

instance InitialisedEvent ModelEvent where
  initialisedEvent = Initialised
