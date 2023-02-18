{-# LANGUAGE CPP #-}

module Development.Guardian.Flags (cabalEnabled, stackEnabled) where

cabalEnabled :: Bool
#if defined(ENABLE_CABAL)
cabalEnabled = True
#else
cabalEnabled = False
#endif

stackEnabled :: Bool
#if defined(ENABLE_STACK)
stackEnabled = True
#else
stackEnabled = False
#endif
