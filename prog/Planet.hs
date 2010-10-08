module Main
 where

import Planet.Worker
import Planet.SimpleBot

main :: IO()
main = 
  loop simpleBot
