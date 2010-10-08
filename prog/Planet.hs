module Main
 where

import Planet.Worker

main :: IO()
main = 
  loop (return [])
