module Planet.ParserTest 
  where

import Planet.Parser
import Planet.Type

prop_parsePlanet :: Planet -> Bool
prop_parsePlanet planet = 
  expectedState == parsedGameState
  where parsedGameState = parseGameElement (show planet)
        expectedState = emptyGameState {planets = [planet]} 

prop_parseFleet :: Fleet -> Bool
prop_parseFleet fleet = 
  expectedState == parsedGameState
  where parsedGameState = parseGameElement (show fleet)
        expectedState = emptyGameState {fleets = [fleet]} 
 
prop_parseGame :: GameState -> Bool
prop_parseGame gameState = 
  gameState == parsedGameState
  where parsedGameState = parseGameState (lines .  show $ gameState)

