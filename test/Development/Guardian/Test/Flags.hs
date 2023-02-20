{-# LANGUAGE CPP #-}

module Development.Guardian.Test.Flags (testGraphmod, testCabalPlan) where

testGraphmod :: Bool
#if defined(TEST_GRAPHMOD)
testGraphmod = True
#else
testGraphmod = False
#endif

testCabalPlan :: Bool
#if defined(TEST_CABAL_PLAN)
testCabalPlan = True
#else
testCabalPlan = False
#endif
