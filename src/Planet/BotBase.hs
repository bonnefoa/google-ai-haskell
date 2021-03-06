module Planet.BotBase 
  where

import Planet.Type 
import Planet.GameHelper
import Control.Monad.State.Lazy
import Data.List
import qualified Data.IntMap as M

sendShip :: Planet -> Planet -> Int -> GameState (Maybe Order)
sendShip src dest numShip 
  | numShip <= 0 = return Nothing
  | numberShip src < numShip  = return Nothing
  | otherwise = return . Just $ Order (planetId src) (planetId dest) numShip

sendShipWithDecisionAlgorithm :: Planet -> ChooseShipAlgorithm -> Planet -> GameState (Maybe Order)
sendShipWithDecisionAlgorithm dest alg src = sendShip src dest (alg src dest) 

getAllPlanets :: GameState [Planet]
getAllPlanets = fmap M.elems (gets planets)

getMyStrongestPlanet :: GameState [Planet]
getMyStrongestPlanet = fmap 
  (take 3 . reverse . sortBy shipNumberOrdering . filter isAlly)
  getAllPlanets

getWeakestPlanet :: GameState Planet
getWeakestPlanet = fmap 
  (head . sortBy shipNumberOrdering . filter isTakable)
  getAllPlanets

currentAllyFleetInMovement :: GameState Int
currentAllyFleetInMovement = fmap (length . filter isAlly) (gets fleets) 

currentEnnemyFleetInMovement :: GameState Int
currentEnnemyFleetInMovement = fmap (length . filter isEnnemy) (gets fleets) 

