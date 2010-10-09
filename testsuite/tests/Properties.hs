module Planet.Properties  
  where

import Planet.Type
import Test.QuickCheck

instance Arbitrary Ownership where
  arbitrary = elements [Neutral, Ennemy, Ally]

instance Arbitrary GameMap where
  arbitrary = do
    ally_planet <- fmap (\a -> a {planetOwner = Ally} ) arbitrary
    ennemy_planet <- fmap (\a -> a {planetOwner = Ennemy} ) arbitrary
    gen_planets <- fmap (\l -> ally_planet : ennemy_planet : l) (listOf arbitrary)
    let keys = [0.. length gen_planets]
    gen_fleets <- listOf $ generateValidFleets keys
    return $ GameMap (assignIdToPlanets gen_planets) gen_fleets 

generateValidFleets :: [PlanetId] -> Gen Fleet
generateValidFleets keyList = do
  let generator = elements keyList
  src <- generator
  dest <- suchThat generator (/= src) 
  fleet <- arbitrary
  return $ fleet { fleetSrc = src, fleetDest = dest }

instance Arbitrary Planet where 
  arbitrary = do
    let gen_planetId = 0 
    gen_planetX <- generateSmallDouble 
    gen_planetY <- generateSmallDouble 
    gen_planetOwner <- arbitrary
    gen_planetNumberShip <- generateSmallPositiveInt
    gen_planetGrowthRate <- generateSmallPositiveInt
    return $ Planet gen_planetId gen_planetX gen_planetY gen_planetOwner gen_planetNumberShip gen_planetGrowthRate

instance Arbitrary Fleet where 
  arbitrary = do
    gen_fleetOwner <- arbitrary
    gen_fleetNumberShip <- generateSmallPositiveInt
    gen_fleetSrc <- generateSmallPositiveInt
    gen_fleetDst <- generateSmallPositiveInt
    gen_fleetTotalTurns <- generateSmallPositiveInt
    gen_fleetRemainingTurns <- generateSmallPositiveInt
    return $ Fleet gen_fleetNumberShip gen_fleetSrc gen_fleetDst gen_fleetTotalTurns gen_fleetOwner gen_fleetRemainingTurns

generateSmallPositiveInt :: Gen Int
generateSmallPositiveInt = do 
  elements [1..1000]

generateSmallDouble :: Gen Double 
generateSmallDouble = do
  num <- elements [1..1000]
  den <- elements [1,2,4,10]
  return $ num / den

