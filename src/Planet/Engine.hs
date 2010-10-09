module Planet.Engine where

import Planet.Type
import Planet.GameHelper
import qualified Data.IntMap as M
import Control.Monad.State.Lazy


{-
departure :: [Order] -> GameState ()
departure orders = modify fun
  where fun gameState = 
          (\order -> M.adjust (\pl -> pl -+ (orderNumberShip order) ) ) orders
          -}


-- resolveConflict :: GameState ()

-- * Fleet management 

departFleet :: Order -> M.IntMap Planet -> M.IntMap Planet
departFleet order = M.adjust funMod key
  where key = orderSrc order
        funMod pl = pl -+ orderNumberShip order

createFleet :: M.IntMap Planet -> Order -> Ownership -> Fleet
createFleet mapPlanet order orderOwner = Fleet {
    fleetSrc = orderSrc order
   ,fleetDest = orderDest order
   ,fleetOwner = orderOwner
   ,fleetNumberShip = orderNumberShip order
   ,fleetTotalTripLength = fleetTripLength
   ,fleetRemainingTripLength = fleetTripLength
  }
  where fleetTripLength = tripLength planetSrc planetDest
        planetSrc = mapPlanet M.! orderSrc order
        planetDest = mapPlanet M.! orderDest order

advanceFleets :: [Fleet] -> [Fleet]
advanceFleets = map advanceFleet

-- * Planet modifications 

applyGrowthRate :: M.IntMap Planet -> M.IntMap Planet
applyGrowthRate = M.map applyGrowthRate'
  where applyGrowthRate' pl
                  | isOccupied pl = pl -+ planetGrowthRate pl
                  | otherwise = pl

modifyAllPlanet :: (Planet -> Planet) -> GameState ()
modifyAllPlanet f = modify fun
  where fun gameMap = gameMap {planets = M.map f (planets gameMap) }

modifyPlanet :: PlanetId -> (Planet -> Planet) -> GameState ()
modifyPlanet key f = modify fun
  where fun gameMap = gameMap {planets = M.adjust f key (planets gameMap) }

