module GameWithStateT (play) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (StateT, get, put)
import Data.Foldable (traverse_)
import Domain (Guess, Outcome (..), Player (..))
import Utils (guessNumber, printLastGuess, printSeparator)

getOutcome :: Guess -> [Player] -> Outcome
getOutcome houseGuess players =
  let outcome = filter (\(Player _ guesses) -> houseGuess `elem` guesses) players
   in case length outcome of
        0 -> None
        1 -> Winner $ head outcome
        _ -> Tie outcome

playRound :: Guess -> StateT [Player] IO Outcome
playRound houseGuess = do
  players <- get
  players' <- liftIO $ traverse (printLastGuess <=< guessNumber) players
  put players'
  return $ getOutcome houseGuess players'

play :: Guess -> StateT [Player] IO Outcome
play houseGuess = do
  outcome <- playRound houseGuess
  case outcome of
    Winner (Player name guesses) ->
      do
        traverse_
          liftIO
          [ printSeparator,
            putStrLn $ name ++ " has won with " ++ show (length guesses) ++ " guesses!!\n"
          ]
        return outcome
    Tie (Player _ guesses : _) ->
      do
        traverse_
          liftIO
          [ printSeparator,
            putStrLn $ "We have a tie after " ++ show (length guesses) ++ " guesses!!\n"
          ]
        return outcome
    _ -> play houseGuess
