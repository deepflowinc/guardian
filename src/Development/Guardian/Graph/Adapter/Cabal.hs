{-# LANGUAGE CPP #-}

module Development.Guardian.Graph.Adapter.Cabal (module Cabal) where
#if defined(ENABLE_CABAL)
import Development.Guardian.Graph.Adapter.Cabal.Enabled as Cabal
#else
import Development.Guardian.Graph.Adapter.Cabal.Disabled as Cabal
#endif
