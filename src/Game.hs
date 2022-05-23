module Game (play) where

import Control.Monad ((<=<))
import Domain (Game (..), Outcome (..), Player (Player))
import Utils (guessNumber, printLastGuess, printSeparator)

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
