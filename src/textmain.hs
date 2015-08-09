{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.IO
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Game
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Char
import Data.String.Utils

data TextGameConfig = TextGameConfig { filename :: String, timeLimit :: Int, memLimit :: Int,
                                                   coresAvailable :: Int, powerPhrases :: [String] }

powerPhraseChars = [ ('p', "p'!.03"), ('b',"bcefy2"), ('a',"aghij4"), ('l', "lmno 5"),
                     ('d', "dqrvz1"), ('k', "kstuwx") ]

powerPhraseLookup =
    Map.fromList $ concatMap makePowerPhrasePairs powerPhraseChars
        where
          makePowerPhrasePairs (p,cs) = map (\c -> (c,p)) cs

parseArgs [] config = config
parseArgs ("-f":f:ss) config = parseArgs ss (config { filename = f })
parseArgs ("-t":t:ss) config = parseArgs ss (config { timeLimit = read t })
parseArgs ("-m":m:ss) config = parseArgs ss (config { memLimit = read m })
parseArgs ("-c":c:ss) config = parseArgs ss (config { coresAvailable = read c })
parseArgs ("-p":p:ss) config = parseArgs ss (config { powerPhrases = p : (powerPhrases config) })
parseArgs (s:ss) config = parseArgs ss config

newConfig = TextGameConfig "" 0 0 0 []

powerPhraseToCanonical [] = []
powerPhraseToCanonical (p:pp) =
    if isNothing mappedChar then
        p : powerPhraseToCanonical pp
    else
        (fromJust mappedChar) : powerPhraseToCanonical pp
            where
              mappedChar = Map.lookup (toLower p) powerPhraseLookup

canonicalPowerPhrases [] = []
canonicalPowerPhrases (p:pp) =
    (powerPhraseToCanonical p, p) : canonicalPowerPhrases pp

replacePowerPhrases [] commands = commands
replacePowerPhrases ((c,p):cl) commands =
    if isInfixOf c commands then
        replacePowerPhrases ((c,p):cl) (replace c p commands)
    else
        replacePowerPhrases cl commands
    
findPowerPhrases canonicalList sol =
    sol { solution = replacePowerPhrases canonicalList (solution sol) }

gameToSolution config game =
    GameSolution (Game.id config) (game_seed game) (moveListToString (reverse $ move_list game))

moveListToString [] = []
moveListToString (MOVE_W:ms) = 'p':(moveListToString ms)
moveListToString (MOVE_E:ms) = 'b':(moveListToString ms)
moveListToString (MOVE_SW:ms) = 'a':(moveListToString ms)
moveListToString (MOVE_SE:ms) = 'l':(moveListToString ms)
moveListToString (MOVE_CLOCKWISE:ms) = 'd':(moveListToString ms)
moveListToString (MOVE_COUNTERCLOCKWISE:ms) = 'k':(moveListToString ms)

main = do
  args <- getArgs
  let textGameConfig = parseArgs args newConfig
  fileData <- readFile (filename textGameConfig)
  let config = decodeJSON fileData :: GameConfig
  let games = initGames config
  let finished_games = map runGame games
  let solutions = map (gameToSolution config) finished_games
  let powerCanonical = canonicalPowerPhrases $ powerPhrases textGameConfig
  let powerSolutions = map (findPowerPhrases powerCanonical) solutions
  putStrLn $ encodeJSON powerSolutions

    

