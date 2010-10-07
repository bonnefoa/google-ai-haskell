{-# LANGUAGE TemplateHaskell #-}
module Main
 where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

main :: IO()
main = defaultMain $(thListTestFramework)

