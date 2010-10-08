module Planet.Type 
  where

import Data.Monoid (Monoid, mempty, mappend)
import Data.List

data Ownership = Ally | Ennemy | Neutral
  deriving (Eq)

type PlanetId = Integer

data GameState = GameState {
  planets :: [Planet]
  ,fleets :: [Fleet]
 } deriving (Eq)

instance Monoid GameState where
    mempty = GameState mempty mempty
    mappend (GameState p1 f1) (GameState p2 f2) =
        GameState (p1 `mappend` p2) (f1 `mappend` f2)

data Planet = Planet {
  planetId :: PlanetId
  ,planetX :: Double
  ,planetY :: Double
  ,planetOwner :: Ownership
  ,planetNumberShip :: Integer
  ,planetGrowthRate :: Integer
 } deriving (Eq)

data Fleet = Fleet {
  fleetSrc :: PlanetId
  ,fleetDest :: PlanetId
  ,fleetTotalTripLength :: Integer
  ,fleetRemainingTripLength :: Integer
  ,fleetOwner :: Ownership
  ,fleetNumberShip :: Integer
 } deriving (Eq)

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

instance Show GameState where
  show game = intercalate "\n" ( (map show . planets) game ++ (map show . fleets) game )

instance Show Planet where
  show planet = intercalate " " ("P" :list)
    where list = map (\f -> f planet) [show . planetX, show . planetY, show . planetOwner, show . planetNumberShip, show . planetGrowthRate] 

instance Show Fleet where
  show fleet = intercalate " " ("F" :list)
    where list = map (\f -> f fleet) [show . fleetOwner, show . fleetNumberShip, show . fleetSrc, show . fleetDest, show . fleetTotalTripLength, show . fleetRemainingTripLength] 

