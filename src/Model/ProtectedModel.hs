module Model.ProtectedModel
   ( ProtectedModel
   , onEvent
   , waitFor
   , module Exported
   )
  where

import Hails.MVC.Model.ProtectedModel.Initialisation as Exported
import Model.ProtectedModel.ProtectedModelInternals
import Model.ReactiveModel.ModelEvents     as Exported
import Model.ProtectedModel.Status         as Exported
