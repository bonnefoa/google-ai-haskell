module Planet.Parser 
  where

import Planet.Type
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Debug.Trace 

gameStateParser :: String -> GameState
gameStateParser input = either (\err -> trace (show err) emptyGameState ) id $ parse gameStateParser' "parseGameState" input

gameStateParser' :: GenParser Char st GameState
gameStateParser' = 
  many1 parsePlanet >>= \parsedPlanets ->
  return $ GameState parsedPlanets []

parsePlanet :: GenParser Char st Planet
parsePlanet = char 'P' >> spaces >>
  parseFloat >>= \x -> spaces >>
  parseFloat >>= \y -> spaces >>
  parseOwner >>= \owner ->  spaces >>
  parseInt >>= \numberShip -> spaces >>
  parseInt >>= \growthRate ->
  return (Planet 0 x y owner numberShip growthRate)

parseInt :: GenParser Char st Integer
parseInt = parseSign >>= \sign ->
  fmap (sign ) ( P.integer (P.makeTokenParser emptyDef) )

parseSign ::Num a => GenParser Char st (a -> a)
parseSign = option id $
  choice [ char '-' >> return negate
    ,char '+' >> return id ]

parseFloat :: GenParser Char st Double
parseFloat = parseSign >>= \sign ->
  choice [
    fmap sign ( P.float (P.makeTokenParser emptyDef ) ) 
    ,fmap (sign . fromIntegral ) (P.integer (P.makeTokenParser emptyDef)) 
  ]

parseOwner :: GenParser Char st Ownership
parseOwner = choice [
  char '0' >> return Neutral,
  char '1' >> return Ally,
  char '2' >> return Ennemy
 ]

