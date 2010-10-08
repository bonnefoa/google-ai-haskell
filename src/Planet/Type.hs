{-# LANGUAGE FlexibleInstances #-}
module Planet.Type 
  where

import Control.Monad.State.Lazy
import Data.Monoid (Monoid, mempty, mappend)
import Data.List
import qualified Data.IntMap as M

type PlanetState = State GameState

data Ownership = Ally | Ennemy | Neutral
  deriving (Eq, Show)

type PlanetId = Int

type Bot = PlanetState [Order] 

class Resource a where
  owner :: a -> Ownership
  numberShip :: a -> Int

class Serialize a where
  serialize :: a -> String

type ChooseShipAlgorithm = Planet -> Planet -> Int

data AttackData = AttackData {
  attackingPlanets :: [Planet]
  ,attackedPlanet :: Planet
  ,chooseShip :: ChooseShipAlgorithm
 }

data GameState = GameState {
  planets :: M.IntMap Planet
  ,fleets :: [Fleet]
 } deriving (Eq, Show)

instance Monoid GameState where
   mempty = GameState mempty mempty 
   mappend (GameState p1 f1) (GameState p2 f2) =
       GameState (p1 `mappend` p2) (f1 `mappend` f2) 

data ParsedElements = ParsedElements {
    parsedPlanets :: [Planet]
    ,parsedFleets :: [Fleet]
 } deriving (Eq,Show)


instance Monoid ParsedElements where
  mempty = ParsedElements mempty mempty
  mappend (ParsedElements p1 f1) (ParsedElements p2 f2) = 
    ParsedElements (p1 `mappend` p2) (f1 `mappend` f2)

data Planet = Planet {
  planetId :: PlanetId
  ,planetX :: Double
  ,planetY :: Double
  ,planetOwner :: Ownership
  ,planetNumberShip :: Int
  ,planetGrowthRate :: Int
 } deriving (Eq, Show)

data Fleet = Fleet {
  fleetSrc :: PlanetId
  ,fleetDest :: PlanetId
  ,fleetTotalTripLength :: Int
  ,fleetRemainingTripLength :: Int
  ,fleetOwner :: Ownership
  ,fleetNumberShip :: Int
 } deriving (Eq, Show)

data Order = Order {
  orderSrc :: PlanetId
  ,orderDest :: PlanetId
  ,orderNumberShip :: Int
 } deriving (Eq, Show)

assignIdToPlanets :: [Planet] -> M.IntMap Planet
assignIdToPlanets planetList = M.fromList $ map (\(tupleId,planet) -> (tupleId, planet{planetId = tupleId}) )  tuples
  where tuples = zip [0..] planetList

instance Serialize Ownership where
  serialize Neutral = "0"
  serialize Ally = "1"
  serialize Ennemy = "2" 

instance Serialize Order where
  serialize order = intercalate " " list
    where list = map (\f -> f order)  [show . orderSrc, show . orderDest, show . orderNumberShip]

instance Serialize GameState where
  serialize game = intercalate "\n" ( map serialize (M.elems $ planets game) ++ (map serialize . fleets) game )

instance Serialize Planet where
  serialize planet = intercalate " " ("P" :list)
    where list = map (\f -> f planet) [show . planetX, show . planetY, serialize . planetOwner, show . planetNumberShip, show . planetGrowthRate] 

instance Serialize [Planet] where
  serialize list = intercalate "\n" (map serialize list)  

instance Resource Planet where
  owner = planetOwner
  numberShip = planetNumberShip

instance Resource Fleet where
  owner = fleetOwner 
  numberShip = fleetNumberShip

instance Serialize Fleet where
  serialize fleet = intercalate " " ("F" :list)
    where list = map (\f -> f fleet) [serialize . fleetOwner, show . fleetNumberShip, show . fleetSrc, show . fleetDest, show . fleetTotalTripLength, show . fleetRemainingTripLength] 

instance Serialize [Fleet] where
  serialize list = intercalate "\n" (map serialize list)  

instance Ord Planet where
  r1 <= r2 = numberShip r1 <= numberShip r2

