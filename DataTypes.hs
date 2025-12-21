{-# LANGUAGE DeriveGeneric #-}

module DataTypes where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- Guess Game 
data GuessState = GuessState
  { secret   :: Int
  , attempts :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON GuessState
instance FromJSON GuessState

data GuessResult
  = TooLow
  | TooHigh
  | Correct
  deriving (Show, Eq, Generic)

instance ToJSON GuessResult
instance FromJSON GuessResult

-- Tic Tac Toe
data Player = X | O
  deriving (Show, Eq, Generic)

instance ToJSON Player
instance FromJSON Player

type Board = [Maybe Player]

data TTTState = TTTState
  { board   :: Board
  , current :: Player
  } deriving (Show, Eq, Generic)

instance ToJSON TTTState
instance FromJSON TTTState

data TTTResult
  = Win Player
  | Draw
  | Ongoing
  deriving (Show, Eq, Generic)

instance ToJSON TTTResult
instance FromJSON TTTResult

-- Hangman
data HangmanState = HangmanState
  { word    :: String
  , guessed :: [Char]
  , lives   :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON HangmanState
instance FromJSON HangmanState

data HangmanResult
  = HangmanWin
  | HangmanLose
  | HangmanOngoing
  deriving (Show, Eq, Generic)

instance ToJSON HangmanResult
instance FromJSON HangmanResult
