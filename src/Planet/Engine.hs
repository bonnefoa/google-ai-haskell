module Planet.Engine where

import Planet.Type

distanceBetween :: Planet -> Planet -> Double
distanceBetween p1 p2 = sqrt $ dx * dx + dy * dy
  where dx = planetX p1 - planetX p2
        dy = planetY p1 - planetY p2

applyGrowthRate :: GameState ()
applyGrowthRate = undefined

