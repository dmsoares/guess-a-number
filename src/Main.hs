module Main where

import Data.Maybe
import qualified System.Random as SR

data Player = Player Name [Guess]
  deriving (Show)

data GameState = Game Guess [Player]
  deriving (Show)

data Outcome = None | Tie [Player] | Winner Player
  deriving (Show)

type Name = String

type Guess = Int

mkPlayer :: String -> Maybe Player
mkPlayer name
  | null name = Nothing
  | otherwise = Just $ Player name []

mkPlayers :: [String] -> [Player]
mkPlayers = mapMaybe mkPlayer

getRandomGuess :: IO Guess
getRandomGuess = SR.randomRIO (0 :: Int, 10)

guessNumber :: Player -> IO Player
guessNumber p@(Player name guesses) = do
  n <- getRandomGuess
  return $ Player name $ guesses ++ [n]

printLastGuess :: Player -> IO ()
printLastGuess p@(Player name guesses) =
  putStrLn (name ++ " has guessed " ++ show (last guesses))

printSeparator :: IO ()
printSeparator = putStrLn $ replicate 12 '-'

getOutcome :: GameState -> Outcome
getOutcome (Game guess players) =
  let outcome = filter (\(Player _ guesses) -> guess `elem` guesses) players
   in case length outcome of
        0 -> None
        1 -> Winner $ head outcome
        _ -> Tie outcome

play :: GameState -> IO ()
play (Game guess players) = do
  players' <- traverse guessNumber players
  traverse printLastGuess players'
  let newState = Game guess players'
  case getOutcome newState of
    Winner (Player name guesses) ->
      do
        printSeparator
        putStrLn $ name ++ " has won with " ++ show (length guesses) ++ " guesses!!"
    Tie (Player _ guesses : players) ->
      do
        printSeparator
        putStrLn $ "We have a tie after " ++ show (length guesses) ++ " guesses!!"
    _ -> play newState

main :: IO ()
main = do
  houseGuess <- getRandomGuess
  putStrLn $ "House picks " ++ show houseGuess
  printSeparator
  play $ Game houseGuess $ mkPlayers ["Decio", "Sara", "", "Ana"]
