{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.IO
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Game

gameToSolution config game =
    GameSolution (Game.id config) (game_seed game) (moveListToString (move_list game))

moveListToString [] = []
moveListToString (MOVE_W:ms) = 'p':(moveListToString ms)
moveListToString (MOVE_E:ms) = 'b':(moveListToString ms)
moveListToString (MOVE_SW:ms) = 'a':(moveListToString ms)
moveListToString (MOVE_SE:ms) = 'l':(moveListToString ms)
moveListToString (MOVE_CLOCKWISE:ms) = 'd':(moveListToString ms)
moveListToString (MOVE_COUNTERCLOCKWISE:ms) = 'k':(moveListToString ms)

main = do
  args <- getArgs
  fileData <- readFile (head args)
  let config = decodeJSON fileData :: GameConfig
  let games = initGames config
  let finished_games = map runGame games
  let solutions = map (gameToSolution config) finished_games
  putStrLn $ encodeJSON solutions

    

