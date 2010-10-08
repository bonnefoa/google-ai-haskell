module Planet.SimpleBot where

import Planet.BotBase 
import Planet.Type

theHeart :: Bot
theHeart = do 
  act <- currentAllyFleetInMovement 
  if act > 1 
     then return []
     else attack
       
attack :: Bot
attack = 
  getWeakestPlanet >>= \dest ->
  getMyStrongestPlanet >>= \listSrc ->
  mapM (sendShipWithDecisionAlgorithm dest chooseNumberShip) listSrc

chooseNumberShip :: Planet -> Planet -> Int
chooseNumberShip src _ = (`div` 2) . numberShip $ src

