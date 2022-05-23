module Domain where

import Data.Maybe (mapMaybe)

data Player = Player Name [Guess]
  deriving (Show)

data Game = Game Guess [Player]
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
