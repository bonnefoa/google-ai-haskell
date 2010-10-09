module Planet.EngineTest where

import Planet.Type
import Planet.Engine
import Planet.GameHelper

prop_applyGrowthRate :: GameMap -> Bool
prop_applyGrowthRate gameMap = 
  getTotalShipsFromPlanets newMapPl > getTotalShipsFromPlanets mapPl
  where mapPl = planets gameMap 
        newMapPl = applyGrowthRate mapPl


