module Planet.Properties  
  where

import Planet.Type
import Test.QuickCheck
import Data.Ratio

instance Arbitrary Ownership where
  arbitrary = elements [Neutral, Ennemy, Ally]

instance Arbitrary GameState where
  arbitrary = do
    gen_fleets <- listOf arbitrary
    gen_planets <- listOf arbitrary
    return $ GameState gen_planets gen_fleets

instance Arbitrary Planet where 
  arbitrary = do
    let gen_planetId =0 
    gen_planetX <- generateSmallDouble 
    gen_planetY <- generateSmallDouble 
    gen_planetOwner <- arbitrary
    gen_planetNumberShip <- arbitrary
    gen_planetGrowthRate <- arbitrary
    return $ Planet gen_planetId gen_planetX gen_planetY gen_planetOwner gen_planetNumberShip gen_planetGrowthRate

instance Arbitrary Fleet where 
  arbitrary = do
    gen_fleetOwner <- arbitrary
    gen_fleetNumberShip <- arbitrary
    gen_fleetSrc <- arbitrary
    gen_fleetDst <- arbitrary
    gen_fleetTotalTurns <- arbitrary
    gen_fleetRemainingTurns <- arbitrary
    return $ Fleet gen_fleetOwner gen_fleetNumberShip gen_fleetSrc gen_fleetDst gen_fleetTotalTurns gen_fleetRemainingTurns

generateSmallDouble :: Gen Double 
generateSmallDouble = do
  num <- elements [1..1000]
  den <- elements [1,2,4,5,6,8,10]
  return $ fromRational $ num % den

