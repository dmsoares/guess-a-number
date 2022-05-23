module Utils where

import Domain (Guess, Player (..))
import qualified System.Random as SR

getRandomGuess :: IO Guess
getRandomGuess = SR.randomRIO (0 :: Int, 10)

guessNumber :: Player -> IO Player
guessNumber (Player name guesses) = do
  n <- getRandomGuess
  return $ Player name $ guesses ++ [n]

printLastGuess :: Player -> IO Player
printLastGuess p@(Player name guesses) =
  putStrLn (name ++ " has guessed " ++ show (last guesses)) >> return p

printSeparator :: IO ()
printSeparator = putStrLn $ replicate 12 '-'
