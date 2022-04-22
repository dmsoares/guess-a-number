module Main where

import qualified System.Random as SR

data Player = Player Name Guesses
  deriving (Show)

data Outcome = None | Tie [Player] | Winner Player
  deriving (Show)

type Name = String

type Guesses = [Int]

type GameState = [Player]

guessNumber :: Player -> IO Player
guessNumber p@(Player name guesses) = do
  n <- SR.randomRIO (0 :: Int, 10)
  if n `elem` guesses
    then guessNumber p
    else return $ Player name $ guesses ++ [n]

printLastGuess :: Player -> IO Player
printLastGuess p@(Player name guesses) =
  putStrLn (name ++ " has guessed " ++ show (last guesses)) >> return p

getOutcome :: Int -> GameState -> Outcome
getOutcome n state =
  let outcome = filter (\(Player _ guesses) -> n `elem` guesses) state
   in case length outcome of
        0 -> None
        1 -> Winner $ head outcome
        _ -> Tie outcome

play :: Int -> GameState -> IO ()
play n state = do
  state' <- traverse guessNumber state
  traverse printLastGuess state'
  let outcome = getOutcome n state'
  case outcome of
    Winner (Player name guesses) ->
      putStrLn $ name ++ " has won with " ++ show (length guesses) ++ " guesses!!"
    Tie (Player _ guesses : players) ->
      putStrLn $ "We have a tie after " ++ show (length guesses) ++ " guesses!!"
    _ -> play n state'
  return ()

main :: IO ()
main = do
  let initialState = [Player "Decio" [], Player "Sara" []]
  houseGuess <- SR.randomRIO (0 :: Int, 10)
  putStrLn $ "House guess: " ++ show houseGuess
  play houseGuess initialState
