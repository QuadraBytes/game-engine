{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.Wai.Handler.Warp (runSettings, setPort, setHost, defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Monad.IO.Class (liftIO)

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

  -- ✅ Log every HTTP request (method, path, status)
  middleware logStdoutDev

  -- ✅ CORS
  middleware $ cors $ const $ Just CorsResourcePolicy
   { corsOrigins = Nothing  
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

    -- ✅ Log request parameters
    liftIO $
      putStrLn $
        "[REQUEST] POST /guess | guessValue="
          ++ show (guessValue req)
          ++ " | guessState="
          ++ show (guessState req)

    let (res, newState) =
          runGuessAPI (guessValue req) (guessState req)

    json $ GuessResponse res newState

  -- ---------------------------
  -- Tic Tac Toe
  -- ---------------------------
  post "/ttt/move" $ do
    req <- jsonData

    -- ✅ Log request parameters
    liftIO $
      putStrLn $
        "[REQUEST] POST /ttt/move | moveIndex="
          ++ show (moveIndex req)
          ++ " | tttState="
          ++ show (tttState req)

    case runTTTMove (moveIndex req) (tttState req) of
      Nothing -> do
        liftIO $ putStrLn "[RESULT] Invalid TTT move"
        text "Invalid move"
      Just (newState, result) -> do
        liftIO $
          putStrLn $
            "[RESULT] TTT result="
              ++ show result
        json $ TTTResponse newState result

  -- ---------------------------
  -- Hangman
  -- ---------------------------
  post "/hangman/guess" $ do
    req <- jsonData

    -- ✅ Log request parameters
    liftIO $
      putStrLn $
        "[REQUEST] POST /hangman/guess | guessedLetter="
          ++ show (guessedLetter req)
          ++ " | hangmanState="
          ++ show (hangmanState req)

    let (newState, result) =
          runHangmanAPI (guessedLetter req) (hangmanState req)

    json $ HangmanResponse newState result
