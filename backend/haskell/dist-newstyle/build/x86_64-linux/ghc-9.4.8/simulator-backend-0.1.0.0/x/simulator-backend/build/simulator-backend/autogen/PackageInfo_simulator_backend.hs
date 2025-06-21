{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_simulator_backend (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "simulator_backend"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Solar System simulator backend"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
