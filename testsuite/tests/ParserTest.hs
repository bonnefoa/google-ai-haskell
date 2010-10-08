module Planet.ParserTest 
  where

import Planet.Parser
import Planet.Type
import Data.Monoid

prop_parsePlanet :: [Planet] -> Bool
prop_parsePlanet planet = 
  expectedState == parsedGameState
  where parsedGameState = parseGameElements (serialize planet)
        expectedState = mempty {parsedPlanets = planet} 

prop_parseFleet :: [Fleet] -> Bool
prop_parseFleet fleet = 
  expectedState == parsedElement
  where parsedElement = parseGameElements (serialize fleet)
        expectedState = mempty {parsedFleets = fleet} 
 
prop_parseGame :: GameState -> Bool
prop_parseGame gameState = 
  gameState == parsedGameState
  where parsedGameState = parseGameState (serialize gameState)

