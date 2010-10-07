module Planet.ParserTest 
  where

import Planet.Parser
import Planet.Type

prop_parsePlanet :: Planet -> Bool
prop_parsePlanet planet = 
  expectedState == parsedGameState
  where parsedGameState = gameStateParser (show planet)
        expectedState = emptyGameState {planets = [planet]} 

