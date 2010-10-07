module Planet.Type 
  where

import Data.List

data Ownership = Ally | Ennemy | Neutral
  deriving (Eq)

type PlanetId = Integer

data GameState = GameState {
  planets :: [Planet]
  ,fleets :: [Fleet]
 } deriving (Eq, Show)

data Planet = Planet {
  planetId :: PlanetId
  ,planetX :: Double
  ,planetY :: Double
  ,planetOwner :: Ownership
  ,planetNumberShip :: Integer
  ,planetGrowthRate :: Integer
 }

data Fleet = Fleet {
  fleetSrc :: PlanetId
  ,fleetDest :: PlanetId
  ,fleetTotalTripLength :: Integer
  ,fleetRemainingTripLength :: Integer
  ,fleetOwner :: Ownership
  ,fleetNumberShip :: Integer
 } deriving (Eq, Show)

data Order = Order {
  orderSrc :: PlanetId
  ,orderDest :: PlanetId
  ,orderNumberShip :: Integer
 } deriving (Eq)

emptyGameState :: GameState
emptyGameState = GameState [] [] 

instance Show Ownership where
  show Neutral = "0"
  show Ally = "1"
  show Ennemy = "2" 

instance Show Planet where
  show planet = intercalate " " ("P" :list)
    where list = map (\f -> f planet) [show . planetX, show . planetY, show . planetOwner, show . planetNumberShip, show . planetGrowthRate] 

instance Eq Planet where
  planet1 == planet2 = planetX planet1 == planetX planet2 && planetY planet1 == planetY planet2

