module Main where

import qualified System.Random as SR

data Player = Player Name [Guess]
  deriving (Show)

data Outcome = None | Tie [Player] | Winner Player
  deriving (Show)

type Name = String

type Guess = Int

type GameState = [Player]

getRandomGuess :: IO Guess
getRandomGuess = SR.randomRIO (0 :: Int, 10)

guessNumber :: Player -> IO Player
guessNumber p@(Player name guesses) = do
  n <- getRandomGuess
  if n `elem` guesses
    then guessNumber p
    else return $ Player name $ guesses ++ [n]

printLastGuess :: Player -> IO Player
printLastGuess p@(Player name guesses) =
  putStrLn (name ++ " has guessed " ++ show (last guesses)) >> return p

getOutcome :: Guess -> GameState -> Outcome
getOutcome n state =
  let outcome = filter (\(Player _ guesses) -> n `elem` guesses) state
   in case length outcome of
        0 -> None
        1 -> Winner $ head outcome
        _ -> Tie outcome

play :: Guess -> GameState -> IO ()
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
  houseGuess <- getRandomGuess
  putStrLn $ "House guess: " ++ show houseGuess
  play houseGuess initialState
