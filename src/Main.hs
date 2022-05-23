module Main where

import Control.Monad.Trans.State.Lazy (runStateT)
import Domain
import Game
import qualified GameWithStateT as GS

main :: IO ()
main = do
  houseGuess <- getRandomGuess
  putStrLn $ "\nHouse picks " ++ show houseGuess
  printSeparator
  runStateT (GS.play houseGuess) (mkPlayers ["Ada", "Grace", "", "Margaret"])
  return ()

-- main :: IO ()
-- main = do
--   houseGuess <- getRandomGuess
--   putStrLn $ "House picks " ++ show houseGuess
--   printSeparator
--   play $ Game houseGuess $ mkPlayers ["Ada", "Grace", "", "Margaret"]
