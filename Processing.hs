module Processing where

import DataTypes
import Utils
import Data.Maybe (isJust)

-- =====================================================
-- Guess Game Logic
-- =====================================================
processGuess :: Int -> GuessState -> (GuessResult, GuessState)
processGuess guess state
  | guess < secret state =
      (TooLow, state { attempts = attempts state + 1 })
  | guess > secret state =
      (TooHigh, state { attempts = attempts state + 1 })
  | otherwise =
      (Correct, state { attempts = attempts state + 1 })

-- =====================================================
-- Tic Tac Toe Logic
-- =====================================================
initialBoard :: Board
initialBoard = replicate 9 Nothing

makeMove :: Int -> TTTState -> Maybe TTTState
makeMove idx state
  | idx < 0 || idx >= 9 = Nothing
  | isJust (board state !! idx) = Nothing
  | otherwise =
      let newBoard =
            take idx (board state)
            ++ [Just (current state)]
            ++ drop (idx + 1) (board state)
       in Just $
            TTTState
              { board = newBoard
              , current = switchPlayer (current state)
              }

checkWinner :: TTTState -> TTTResult
checkWinner state
  | any (allSame X) winPatterns = Win X
  | any (allSame O) winPatterns = Win O
  | all isJust (board state)    = Draw
  | otherwise                   = Ongoing
  where
    winPatterns =
      map (\p -> map ((board state) !!) p)
        [ [0,1,2],[3,4,5],[6,7,8]
        , [0,3,6],[1,4,7],[2,5,8]
        , [0,4,8],[2,4,6]
        ]

    allSame p = all (== Just p)

-- =====================================================
-- Hangman Logic
-- =====================================================
guessLetter :: Char -> HangmanState -> HangmanState
guessLetter c state
  | c `elem` guessed state = state
  | c `elem` word state =
      state { guessed = c : guessed state }
  | otherwise =
      state { guessed = c : guessed state
            , lives = lives state - 1
            }

checkHangman :: HangmanState -> HangmanResult
checkHangman state
  | all (`elem` guessed state) (uniqueChars (word state)) =
      HangmanWin
  | lives state <= 0 =
      HangmanLose
  | otherwise =
      HangmanOngoing
