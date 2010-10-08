module Planet.BotBase 
  where

import Planet.Type 
import Control.Monad.State.Lazy
import qualified Data.IntMap as M

-- attackNearestPlanet :: Planet -> PlanetState Order
-- attackNearestPlanet home = 

getPlanetById :: PlanetId -> PlanetState Planet
getPlanetById pId = do
  theMap <- gets planets 
  return $ theMap M.! pId

modifyPlanet :: PlanetId -> (Planet -> Planet) -> PlanetState ()
modifyPlanet key f = modify 
  ( \gameState -> gameState {planets = M.adjust f key (planets gameState) } )

changeState :: Order -> PlanetState ()
changeState (Order planetSrc planetDest numShip) = 
  -- let src = getPlanetById planetSrc
  -- let dest = getPlanetById planetDest
  return ()

sendShip :: Planet -> Planet -> Int -> PlanetState Order
sendShip src dest numShip = return $ Order (planetId src) (planetId dest) numShip

currentAllyFleetInMovement :: PlanetState Int
currentAllyFleetInMovement = fmap (length . filter isAlly) (gets fleets) 

currentEnnemyFleetInMovement :: PlanetState Int
currentEnnemyFleetInMovement = fmap (length . filter isEnnemy) (gets fleets) 

isAlly :: (Resource a) => a -> Bool
isAlly r = owner r == Ally

isEnnemy :: (Resource a) => a -> Bool
isEnnemy r = owner r == Ennemy

isNeutral :: (Resource a) => a -> Bool
isNeutral r = owner r == Neutral


