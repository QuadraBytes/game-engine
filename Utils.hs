module Utils where

import DataTypes (Player(..))
import Data.Char (toLower)
import Data.List (nub)

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

uniqueChars :: String -> [Char]
uniqueChars = nub . map toLower
