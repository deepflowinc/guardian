{-# LANGUAGE QuasiQuotes #-}

module Development.Guardian.Constants (configFileName) where

import Path (File, Path, Rel, relfile)

configFileName :: Path Rel File
configFileName = [relfile|dependency-domains.yaml|]
