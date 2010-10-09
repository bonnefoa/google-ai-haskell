module Planet.Engine where

import Planet.BotBase
import Planet.Type
import qualified Data.IntMap as M
import Control.Monad.State.Lazy

departure :: [Order] -> GameState ()
departure orders = modify fun
  where fun gameState = 
          (\order -> M.adjust (\pl -> pl -+ (orderNumberShip order) ) ) orders

departFleet :: Order -> GameState ()
departFleet order = 

-- resolveConflict :: GameState ()

distanceBetween :: Planet -> Planet -> Double
distanceBetween p1 p2 = sqrt $ dx * dx + dy * dy
  where dx = planetX p1 - planetX p2
        dy = planetY p1 - planetY p2

tripLength :: Planet -> Planet -> Int
tripLength p1 = ceiling . distanceBetween p1

advanceFleets :: GameState ()
advanceFleets = modify fun
  where fun gameMap = gameMap {fleets = map advanceFleet (fleets gameMap) }

applyGrowthRate :: GameState ()
applyGrowthRate = modifyAllPlanet fun 
  where fun pl | isOccupied pl = pl -+ planetGrowthRate pl
               | otherwise = pl

modifyAllPlanet :: (Planet -> Planet) -> GameState ()
modifyAllPlanet f = modify fun
  where fun gameMap = gameMap {planets = M.map f (planets gameMap) }

modifyPlanet :: PlanetId -> (Planet -> Planet) -> GameState ()
modifyPlanet key f = modify fun
  where fun gameMap = gameMap {planets = M.adjust f key (planets gameMap) }

