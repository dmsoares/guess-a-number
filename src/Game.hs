module Game (play, printSeparator, getRandomGuess) where

import Control.Monad ((<=<))
import Control.Monad.Trans.State
import Data.Maybe (mapMaybe)
import Domain
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

getOutcome :: Game -> Outcome
getOutcome (Game guess players) =
  let outcome = filter (\(Player _ guesses) -> guess `elem` guesses) players
   in case length outcome of
        0 -> None
        1 -> Winner $ head outcome
        _ -> Tie outcome

play :: Game -> IO ()
play (Game guess players) = do
  players' <- traverse (printLastGuess <=< guessNumber) players

  let newState = Game guess players'

  case getOutcome newState of
    Winner (Player name guesses) ->
      do
        printSeparator
        putStrLn $ name ++ " has won with " ++ show (length guesses) ++ " guesses!!\n"
    Tie (Player _ guesses : _) ->
      do
        printSeparator
        putStrLn $ "We have a tie after " ++ show (length guesses) ++ " guesses!!\n"
    _ -> play newState
