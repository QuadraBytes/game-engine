{-# LANGUAGE DeriveGeneric #-}

module ApiTypes where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import DataTypes

-- ===============================
-- Guess Game API
-- ===============================

data GuessRequest = GuessRequest
  { guessValue :: Int
  , guessState :: GuessState
  } deriving (Generic)

instance FromJSON GuessRequest

data GuessResponse = GuessResponse
  { guessResult :: GuessResult
  , updatedGuessState :: GuessState
  } deriving (Generic)

instance ToJSON GuessResponse

-- ===============================
-- Tic Tac Toe API
-- ===============================

data TTTRequest = TTTRequest
  { moveIndex :: Int
  , tttState  :: TTTState
  } deriving (Generic)

instance FromJSON TTTRequest

data TTTResponse = TTTResponse
  { updatedTTTState :: TTTState
  , tttResult       :: TTTResult
  } deriving (Generic)

instance ToJSON TTTResponse

-- ===============================
-- Hangman API
-- ===============================

data HangmanRequest = HangmanRequest
  { guessedLetter :: Char
  , hangmanState  :: HangmanState
  } deriving (Generic)

instance FromJSON HangmanRequest

data HangmanResponse = HangmanResponse
  { updatedHangmanState :: HangmanState
  , hangmanResult       :: HangmanResult
  } deriving (Generic)

instance ToJSON HangmanResponse
