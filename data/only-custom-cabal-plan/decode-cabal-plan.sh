#!/bin/bash
cabal-plan --hide-builtin --hide-global dot 2>/dev/null \
  | sed -r 's/:(test|bench|exe):[^\"]+//g; s/-[0-9]+(\.[0-9]+)*//g'
