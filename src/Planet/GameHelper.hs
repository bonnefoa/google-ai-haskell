module Planet.GameHelper
  where

import Planet.Type
import Control.Monad.State.Lazy
import qualified Data.IntMap as M
import Data.Ord

distanceBetween :: Planet -> Planet -> Double
distanceBetween p1 p2 = sqrt $ dx * dx + dy * dy
  where dx = planetX p1 - planetX p2
        dy = planetY p1 - planetY p2

tripLength :: Planet -> Planet -> Int
tripLength p1 = ceiling . distanceBetween p1

shipNumberOrdering :: Resource a => a -> a -> Ordering
shipNumberOrdering = comparing numberShip 

growthRateOrdering :: Planet -> Planet -> Ordering
growthRateOrdering = comparing planetGrowthRate

getPlanetById :: PlanetId -> GameState Planet
getPlanetById pId = do
  theMap <- gets planets 
  return $ theMap M.! pId

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

getTotalShipsFromPlanets :: M.IntMap Planet -> Int
getTotalShipsFromPlanets = M.fold (\pl num -> planetNumberShip pl + num ) 0


