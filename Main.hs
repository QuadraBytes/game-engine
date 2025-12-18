{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, defaultSettings)

import ApiTypes
import IOHandler

main :: IO ()
main = do
  let settings =
        setPort 3001 $
        setHost "0.0.0.0" $
        defaultSettings

  runSettings settings =<< scottyApp app

app :: ScottyM ()
app = do

  -- ✅ CORS
  middleware $ cors $ const $ Just CorsResourcePolicy
    { corsOrigins = Just (["http://localhost:3000"], True)
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type"]
    , corsExposedHeaders = Nothing
    , corsMaxAge = Just 3600
    , corsVaryOrigin = False
    , corsRequireOrigin = False
    , corsIgnoreFailures = False
    }

  -- ---------------------------
  -- Guess Game
  -- ---------------------------
  post "/guess" $ do
    req <- jsonData
    let (res, newState) =
          runGuessAPI (guessValue req) (guessState req)
    json $ GuessResponse res newState

  -- ---------------------------
  -- Tic Tac Toe
  -- ---------------------------
  post "/ttt/move" $ do
    req <- jsonData
    case runTTTMove (moveIndex req) (tttState req) of
      Nothing -> text "Invalid move"
      Just (newState, result) ->
        json $ TTTResponse newState result

  -- ---------------------------
  -- Hangman
  -- ---------------------------
  post "/hangman/guess" $ do
    req <- jsonData
    let (newState, result) =
          runHangmanAPI (guessedLetter req) (hangmanState req)
    json $ HangmanResponse newState result
