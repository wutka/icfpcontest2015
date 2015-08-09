
module Main where

import Graphics.Gloss
import Game
import System.Environment
import System.IO
import Text.JSON
import Text.JSON.Generic
import Data.List

cellColor Empty = black
cellColor Locked = white
cellColor Piece = blue
cellColor Piece_Pivot = green

borderColor = greyN 0.30

cellSide = 20.0
cellXOffset = cellSide * cos 0.52
cellYOffset = cellSide * sin 0.52

cellWidth = 2.0 * cellXOffset
cellHeight = cellSide + cellYOffset

cellOffsets = [(0.0, 0.0), (cellXOffset, -cellYOffset), (2.0 * cellXOffset, 0.0),
               (2.0 * cellXOffset, cellSide), (cellXOffset, cellSide+cellYOffset), 
               (0.0, cellSide), (0.0,0.0) ] 

makeCellPath x y = map makePoint cellOffsets
    where
      makePoint (xoff, yoff) = (x+xoff, y+yoff)
    
drawCell (x,y,cell) =
    pictures [ color (cellColor cell) (polygon cellPath), color borderColor (line cellPath)]
        where
          cellPath = if y `mod` 2 == 0 then
                         makeCellPath (-200 + cellWidth * (fromIntegral x)) (300 - cellHeight * (fromIntegral y))
                     else
                         makeCellPath (-200 + cellWidth * (fromIntegral x) + cellXOffset) (300 - cellHeight * (fromIntegral y))

drawBoard :: [Board] -> Picture
drawBoard [] = Blank
drawBoard (b:bs) =
    pictures $ map drawCell (boardToList b)

modelStep _ t [] = []
modelStep _ t [x] = [x]
modelStep _ t (x:xs) = xs


main = do
  args <- getArgs
  fileData <- readFile (head args)
  let speed = if length args > 1 then (read (args !! 1)) :: Int else 8
  let config = decodeJSON fileData :: GameConfig
  let games = initGames config
  let game1 = head games
  let g2 = runGame game1

  let boardList = reverse $ full_board_history g2
--  putStrLn $ unlines (map show boardList)
  seq (take 1 boardList) (simulate (InWindow "Hextris" (420, 700) (10, 10)) black speed boardList drawBoard modelStep)
