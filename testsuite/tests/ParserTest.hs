module Planet.ParserTest 
  where

import Planet.Parser
import Planet.Type
import Data.Monoid

prop_parsePlanet :: Planet -> Bool
prop_parsePlanet planet = 
  expectedState == parsedGameState
  where parsedGameState = parseGameElement (serialize planet)
        expectedState = mempty {parsedPlanets = [planet]} 

prop_parseFleet :: Fleet -> Bool
prop_parseFleet fleet = 
  expectedState == parsedElement
  where parsedElement = parseGameElement (serialize fleet)
        expectedState = mempty {parsedFleets = [fleet]} 
 
{-
prop_parseGame :: GameState -> Bool
prop_parseGame gameState = 
  gameState == parsedGameState
  where parsedGameState = parseGameState (lines .  serialize $ gameState)
-}
