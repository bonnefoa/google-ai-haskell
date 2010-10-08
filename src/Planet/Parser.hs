module Planet.Parser 
  where

import Planet.Type
import Data.Monoid
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import qualified Data.IntMap as M

import Debug.Trace 

-- parseGameState :: [String] -> GameState
-- parseGameState = mconcat . map (parseGameElement M.empty)

assignIdToPlanets :: [Planet] -> (M.IntMap Planet)
assignIdToPlanets planetList = M.fromList $ map (\(tupleId,planet) -> (tupleId, planet{planetId = tupleId}) )  tuples
  where tuples = (zip [0..] planetList)

gameStateParser' :: GenParser Char st GameState
gameStateParser' = 
  choice [
    parsePlanet >>= \planet -> return $ GameState (M.singleton 0 planet) []
    ,parseFleet >>= \fleet -> return $ GameState mempty [fleet]
  ]

parseGameElements :: String -> ParsedElements
parseGameElements input = 
  either (\err -> trace (show err) mempty) id $
  parse parseGameElements' "parseGameElement" input

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
  parseOwner >>= \owner ->  spaces >>
  parseInt >>= \numberShip -> spaces >>
  parseInt >>= \growthRate ->
  return $ Planet 0 x y owner numberShip growthRate

parseFleet :: GenParser Char st Fleet
parseFleet = char 'F' >> spaces >>
  parseOwner >>= \owner -> spaces >>
  parseInt >>= \numberShip -> spaces >>
  parseInt >>= \planetSrc -> spaces >>
  parseInt >>= \planetDes -> spaces >>
  parseInt >>= \totalTurns -> spaces >>
  parseInt >>= \remainingTurns ->
  return $ Fleet planetSrc planetDes totalTurns remainingTurns owner numberShip

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

