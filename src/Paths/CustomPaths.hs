{-# LANGUAGE CPP #-}
module Paths.CustomPaths
  (module Paths_keera_three_balance_checker
#ifndef linux_HOST_OS
  , module Paths.CustomPaths
#endif
  )
 where

import Paths_keera_three_balance_checker

#ifndef linux_HOST_OS
vendorKey :: String
vendorKey = "Keera Studios"

programKey :: String
programKey = "Keera PayG Balance Checker"
#endif
