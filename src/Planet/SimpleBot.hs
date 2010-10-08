module Planet.SimpleBot where

import Planet.BotBase 
import Planet.Type
import Data.Maybe

simpleBot :: Bot
simpleBot = do 
  fleetAttacking <- currentAllyFleetInMovement 
  if fleetAttacking > 1 
     then return []
     else attack
       
attack :: Bot
attack = 
  getWeakestPlanet >>= \dest ->
  getMyStrongestPlanet >>= \listSrc ->
  fmap catMaybes $ mapM (sendShipWithDecisionAlgorithm dest chooseNumberShip) listSrc

chooseNumberShip :: Planet -> Planet -> Int
chooseNumberShip src _ = (`div` 2) . numberShip $ src

