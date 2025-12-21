{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors

import ApiTypes
import IOHandler

main :: IO ()
main = scotty 3001 $ do

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

  post "/guess" $ do
    req <- jsonData :: ActionM GuessRequest
    let (res, newState) =
          runGuessAPI (guessValue req) (guessState req)
    json $ GuessResponse res newState

  post "/ttt/move" $ do
    req <- jsonData :: ActionM TTTRequest
    case runTTTMove (moveIndex req) (tttState req) of
      Nothing -> text "Invalid move"
      Just (newState, result) ->
        json $ TTTResponse newState result

  post "/hangman/guess" $ do
    req <- jsonData :: ActionM HangmanRequest
    let (newState, result) =
          runHangmanAPI (guessedLetter req) (hangmanState req)
    json $ HangmanResponse newState result
