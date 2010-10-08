module Planet.Worker where

import Planet.Type
import Planet.Parser

import System.IO 
import Data.List
import Control.Monad.State.Lazy

loop :: Bot -> IO()
loop bot = do
  hSetBuffering stdin NoBuffering
  input <- getBatch []
  let orders = evalState bot (parseGameMap (unlines input))
  mapM_ (putStrLn . serialize) orders
  finishTurn 
  loop bot

finishTurn :: IO()
finishTurn = putStrLn "go" >> hFlush stdout

getBatch :: [String] -> IO [String]
getBatch prec = do
  line <- getLine 
  if "go" `isPrefixOf` line 
     then return prec
     else getBatch ( prec ++ [line] )



