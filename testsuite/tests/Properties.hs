module Planet.Properties  
  where

import Planet.Type
import Test.QuickCheck
import Data.Ratio

instance Arbitrary Ownership where
  arbitrary = elements [Neutral, Ennemy, Ally]

instance Arbitrary Planet where 
  arbitrary = do
    let gen_planetId =0 
    gen_planetX <- generateSmallDouble 
    gen_planetY <- generateSmallDouble 
    gen_planetOwner <- arbitrary
    gen_planetNumberShip <- arbitrary
    gen_planetGrowthRate <- arbitrary
    return $ Planet gen_planetId gen_planetX gen_planetY gen_planetOwner gen_planetNumberShip gen_planetGrowthRate

generateSmallDouble :: Gen Double 
generateSmallDouble = do
  num <- elements [1..1000]
  den <- elements [1..10]
  return $ fromRational $ num % den

