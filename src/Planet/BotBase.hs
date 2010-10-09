module Planet.BotBase 
  where

import Planet.Type 
import Control.Monad.State.Lazy
import Data.List
import qualified Data.IntMap as M
import Data.Ord

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

shipNumberOrdering :: Resource a => a -> a -> Ordering
shipNumberOrdering = comparing numberShip 

growthRateOrdering :: Planet -> Planet -> Ordering
growthRateOrdering = comparing planetGrowthRate

getPlanetById :: PlanetId -> GameState Planet
getPlanetById pId = do
  theMap <- gets planets 
  return $ theMap M.! pId

currentAllyFleetInMovement :: GameState Int
currentAllyFleetInMovement = fmap (length . filter isAlly) (gets fleets) 

currentEnnemyFleetInMovement :: GameState Int
currentEnnemyFleetInMovement = fmap (length . filter isEnnemy) (gets fleets) 

isAlly :: (Resource a) => a -> Bool
isAlly r = owner r == Ally

isEnnemy :: (Resource a) => a -> Bool
isEnnemy r = owner r == Ennemy

isNeutral :: (Resource a) => a -> Bool
isNeutral r = owner r == Neutral

isTakable :: (Resource a) => a -> Bool
isTakable r = isEnnemy r || isNeutral r

isOccupied :: (Resource a) => a -> Bool
isOccupied r = isEnnemy r || isAlly r
