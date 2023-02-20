{-# LANGUAGE CPP #-}

module Development.Guardian.Graph.Adapter.Stack (
  module Stack,
) where

#if defined(ENABLE_STACK)
import  Development.Guardian.Graph.Adapter.Stack.Enabled as Stack
#else
import  Development.Guardian.Graph.Adapter.Stack.Disabled as Stack
#endif
