module Planet.Parser 
  where

import Planet.Type
import Data.Monoid
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Debug.Trace 

parseGameMap :: String -> GameMap
parseGameMap input = either (\err -> trace (show err) mempty) id $
  parse parseGameMap' "parseGameMap" input

parseGameElements :: String -> ParsedElements
parseGameElements input = 
  either (\err -> trace (show err) mempty) id $
  parse parseGameElements' "parseGameElement" input

parseGameMap' :: GenParser Char st GameMap
parseGameMap' = parseGameElements' >>= \parsedElements ->
  return $ GameMap (assignIdToPlanets $ parsedPlanets parsedElements) (parsedFleets parsedElements)

parseGameElements' :: GenParser Char st ParsedElements
parseGameElements' = fmap mconcat $ many parseSingleElement

parseSingleElement :: GenParser Char st ParsedElements
parseSingleElement = choice [ 
   parsePlanet >>= \planet -> return $ ParsedElements [planet] []
   ,parseFleet >>= \fleet -> return $ ParsedElements [] [fleet]
   ,skipMany1 (noneOf "PF")  >> return mempty
  ] 

parsePlanet :: GenParser Char st Planet
parsePlanet = char 'P' >> spaces >>
  parseFloat >>= \x -> spaces >>
  parseFloat >>= \y -> spaces >>
  parseOwner >>= \parsed_owner ->  spaces >>
  parseInt >>= \parsed_numberShip -> spaces >>
  parseInt >>= \growthRate ->
  return $ Planet 0 x y parsed_owner parsed_numberShip growthRate

parseFleet :: GenParser Char st Fleet
parseFleet = char 'F' >> spaces >>
  parseOwner >>= \parsed_owner -> spaces >>
  parseInt >>= \parsed_numberShip -> spaces >>
  parseInt >>= \planetSrc -> spaces >>
  parseInt >>= \planetDes -> spaces >>
  parseInt >>= \totalTurns -> spaces >>
  parseInt >>= \remainingTurns ->
  return $ Fleet planetSrc planetDes totalTurns remainingTurns parsed_owner parsed_numberShip

parseInt :: GenParser Char st Int
parseInt = parseSign >>= \sign ->
  fmap (sign . fromIntegral ) ( P.integer (P.makeTokenParser emptyDef) )

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

