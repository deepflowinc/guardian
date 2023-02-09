{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Development.Guardian.App
import Paths_guardian (version)

main :: IO ()
main = defaultMain $$(buildInfoQ version)
