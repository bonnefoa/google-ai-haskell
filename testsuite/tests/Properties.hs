module Planet.Properties  
  where

import Planet.Type
import Test.QuickCheck

instance Arbitrary Ownership where
  arbitrary = elements [Neutral, Ennemy, Ally]

instance Arbitrary Planet where 
  arbitrary = do
    gen_planetId <- arbitrary
    gen_planetX <- arbitrary
    gen_planetY <- arbitrary
    gen_planetOwner <- arbitrary
    gen_planetNumberShip <- arbitrary
    gen_planetGrowthRate <- arbitrary
    return $ Planet gen_planetId (gen_planetX *10)  (gen_planetY * 10) gen_planetOwner gen_planetNumberShip gen_planetGrowthRate

