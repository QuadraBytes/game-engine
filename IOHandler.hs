module IOHandler where

import DataTypes
import Processing

runGuessAPI :: Int -> GuessState -> (GuessResult, GuessState)
runGuessAPI = processGuess

runTTTMove :: Int -> TTTState -> Maybe (TTTState, TTTResult)
runTTTMove idx state =
  case makeMove idx state of
    Nothing -> Nothing
    Just newState -> Just (newState, checkWinner newState)

runHangmanAPI :: Char -> HangmanState -> (HangmanState, HangmanResult)
runHangmanAPI c state =
  let newState = guessLetter c state
   in (newState, checkHangman newState)

