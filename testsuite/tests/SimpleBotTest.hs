module Planet.SimpleBotTest where

import Control.Monad.State.Lazy

import Planet.SimpleBot
import Planet.Type

prop_sendOrderWhenNoFleet :: GameMap -> Bool
prop_sendOrderWhenNoFleet gameState = (not . null) orders 
  where orders = evalState simpleBot gameState {fleets = []}


