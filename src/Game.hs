{-# LANGUAGE DeriveDataTypeable #-}

module Game where

import System.IO
import System.Environment
import Text.JSON
import Text.JSON.Generic
import Data.List as List
import qualified Data.Map as Map
import qualified Data.Vector as V
import ICFPRandom

data Cell = Cell { x :: Int, y :: Int } deriving (Show, Data, Typeable)

data Unit = Unit { members :: [Cell], pivot :: Cell } deriving (Show, Data, Typeable)

data GameConfig = GameConfig { id :: Int, units :: [Unit],
                                   width :: Int, height :: Int,
                                   filled :: [Cell],
                                   sourceLength :: Int, sourceSeeds :: [Int] } 
                  deriving (Show, Data, Typeable)

data GameSolution = GameSolution { problemId :: Int, seed :: Int, solution :: String } deriving (Show, Data, Typeable)

data CellValueType = Empty | Locked | Piece | Piece_Pivot deriving (Eq, Show)

data Board = Board { w :: Int, h :: Int, cells :: V.Vector CellValueType } deriving (Eq, Show)

data BoardUnit = BoardUnit { unit_xy :: [(Int,Int)] } deriving (Eq, Show)
data BoardPiece = BoardPiece { piece_xy :: [(Int,Int)], pivot_loc :: (Int,Int) } deriving Show

data Directions = NW | NE | E | SE | SW | W deriving Show

data RotateDirection = CLOCKWISE | COUNTERCLOCKWISE deriving (Eq, Ord, Show)

data Move = MOVE_E | MOVE_W | MOVE_SE | MOVE_SW | MOVE_CLOCKWISE | MOVE_COUNTERCLOCKWISE deriving (Eq, Show)

data GameData = GameData { board :: Board, rotate_map :: Map.Map (RotateDirection, Int, Int) [Directions],
                           curr_unit :: BoardUnit, curr_unit_xy :: (Int,Int),
                           remaining_units :: [BoardUnit],
                           move_list :: [Move], ls_old :: Int, score :: Int, board_history :: [Board],
                           full_board_history :: [Board], game_seed :: Int
                         } deriving Show
                                    
initBoard :: Int -> Int -> Board
initBoard width height =
    Board width height (V.fromList (replicate (width*height) Empty))

initBoardFromConfig (GameConfig _ _ width height filled _ _) =
    board { cells = (cells board) V.// (map makeUpdate filled) }
        where
          board = initBoard width height
          makeUpdate (Cell x y) = (y * width + x, Locked)
    
initGames gameConfig = map (initGame gameConfig) (sourceSeeds gameConfig)

initGame gameConfig seed =
    placeUnitOnBoard $ initGame' gameConfig seed

initGame' gameConfig seed =
    GameData (initBoardFromConfig gameConfig) rotateMap (head unit_list) unitStart 
              (tail unit_list) [] 0 0 [] [] seed
                  where
                    unitStart = computeUnitStart (width gameConfig) (head unit_list)
                    normalizedUnits = getNormalizedUnits gameConfig
                    numUnits = List.length normalizedUnits
                    rotateMap = computeRotateMap (getMaxRotateDistance normalizedUnits)
                    unit_num_list = map (\n -> n `mod` numUnits) (take (sourceLength gameConfig) (randomNumbers seed))
                    unit_list = map (\n -> normalizedUnits List.!! n) unit_num_list           

computeUnitStart width (BoardUnit p) =
    (-min_x + ((width - (max_x - min_x)) `div` 2), -min_y)
        where
          min_x = List.minimum $ map fst p
          max_x = List.maximum $ map fst p
          min_y = List.minimum $ map snd p


setCell x y v (Board w h cells) =
          Board w h (cells V.// [(y * w + x, v)])

getCell x y (Board w h cells) =
          cells V.! (y * w + x)

normalizeUnit (Unit m (Cell px py)) =
    BoardUnit (map (transposeCellToXY transposeList) m)
        where
          transposeList = getTransposeList (px,py) (0,0)

getNormalizedUnits config =
    map normalizeUnit (units config)

dirNTimes (x,y) dir 0 = (x,y)
dirNTimes (x,y) dir n = dirNTimes (getCellByDir x y dir) dir (n-1)

rotateDir CLOCKWISE NW = E
rotateDir CLOCKWISE NE = SE
rotateDir CLOCKWISE E = SW
rotateDir CLOCKWISE SE = W
rotateDir CLOCKWISE SW = NW
rotateDir CLOCKWISE W = NE
rotateDir COUNTERCLOCKWISE NW = SW
rotateDir COUNTERCLOCKWISE NE = W
rotateDir COUNTERCLOCKWISE E = NW
rotateDir COUNTERCLOCKWISE SE = NE
rotateDir COUNTERCLOCKWISE SW = E
rotateDir COUNTERCLOCKWISE W = SE

nextDir CLOCKWISE NW = NE
nextDir CLOCKWISE NE = E
nextDir CLOCKWISE E = SE
nextDir CLOCKWISE SE = SW
nextDir CLOCKWISE SW = W
nextDir CLOCKWISE W = NW
nextDir COUNTERCLOCKWISE NW = W
nextDir COUNTERCLOCKWISE NE = NW
nextDir COUNTERCLOCKWISE E = NE
nextDir COUNTERCLOCKWISE SE = E
nextDir COUNTERCLOCKWISE SW = SE
nextDir COUNTERCLOCKWISE W = SW

computeRotateMap depth = Map.fromList $ concatMap createRotateMoves [0..depth]

createRotateMoves depth = (createRotateMovesAtDepth CLOCKWISE depth) ++ (createRotateMovesAtDepth COUNTERCLOCKWISE depth)

getRotateMoveList direction depth n startDir = (List.replicate n startDir) ++ (List.replicate (depth-n) (nextDir direction startDir))

createRotateMovesAtDepth direction 0 = [((direction,0,0),[])]
createRotateMovesAtDepth direction depth =
    concatMap createRotateMovesForSection [NW, NE, E, SE, SW, W]
        where
          createRotateMovesForSection dir = createRotateMovesForSection' depth dir (dirNTimes (0,0) dir depth)
          createRotateMovesForSection' 0 dir pos = []
          createRotateMovesForSection' n dir (x,y) = ((direction,x,y), getRotateMoveList direction depth n (rotateDir direction dir)) : (createRotateMovesForSection' (n-1) dir (getCellByDir x y (rotateDir direction dir)))
                     
rotateBoardUnit (BoardUnit points) direction rotateMap =
    BoardUnit (map (rotatePoint rotateMap direction) points)
        where
          rotatePoint rotateMap direction (x,y) = transposeXY (rotateMap Map.! (direction,x,y)) (x,y)
              
                
getDistance p1 p2 =
    List.length $ getTransposeList p1 p2

getMaxBoardUnitDistance (BoardUnit p) =
    List.maximum $ map (getDistance (0,0)) p

getMaxRotateDistance boardUnits =
    List.maximum $ map getMaxBoardUnitDistance boardUnits

getTransposeList (fromx, fromy) (tox, toy) =
    if fromx == tox && fromy == toy then
        []
    else
        bestDir : getTransposeList (getCellByDir fromx fromy bestDir) (tox,toy)
            where
              bestDir = getBestDir (fromx, fromy) (tox, toy)

transposeCell tl (Cell x y) =
    Cell tox toy
        where
          (tox,toy) = transposeXY tl (x,y)

transposeCellToXY tl (Cell x y) = transposeXY tl (x,y)

transposeXY [] (fromx, fromy) = (fromx, fromy)
transposeXY (t:ts) (fromx, fromy) =
    transposeXY ts (getCellByDir fromx fromy t)

getBestDir (fromx,fromy) (tox,toy) =
    if fromy == toy then
        if fromx < tox then E else W
    else if fromy < toy then
             if fromx < tox then SE else SW
         else
             if fromx < tox then NE else NW

getAdjacentCells x y =
    if y `mod` 2 == 0 then
        [(x-1,y-1), (x, y-1), (x+1, y), (x, y+1), (x-1, y+1), (x-1, y)]
    else
        [(x, y-1), (x+1, y-1), (x+1, y), (x+1, y+1), (x, y+1), (x-1, y)]

getCellByDir x y NW = (getAdjacentCells x y) !! 0
getCellByDir x y NE = (getAdjacentCells x y) !! 1
getCellByDir x y E = (getAdjacentCells x y) !! 2
getCellByDir x y SE = (getAdjacentCells x y) !! 3
getCellByDir x y SW = (getAdjacentCells x y) !! 4
getCellByDir x y W = (getAdjacentCells x y) !! 5

getBoardUnitDimensions (BoardUnit p) =
    (xmax - xmin + 1, ymax - ymin + 1)
        where
          xmin = List.minimum $ map fst p
          xmax = List.maximum $ map fst p
          ymin = List.minimum $ map snd p
          ymax = List.maximum $ map snd p
                        
clearRow (Board w h cells) y =
    Board w h (V.update cells (V.imap computeNewLocation cells))
        where
          computeNewLocation pos v =
              let ypos = pos `div` w in
              if ypos < y then
                  (pos + w, v)
              else if ypos == y then
                       (pos `mod` w, Empty)
                   else
                       (pos, v)

isRowFull (Board w h cells) y =
    V.notElem Empty (V.slice (y * w) w cells)

fullRows (Board w h cells) =
    filter (isRowFull (Board w h cells)) [0..h-1]

clearFullRows b =
    foldl' clearRow b (fullRows b) 

boardToList (Board w h cells) =
    V.toList $ V.imap labelCells cells
     where
       labelCells i c = (i `mod` w, i `div` w, c)
    
removeUnitFromBoard gameData =
    gameData { board = foldl' removeUnitSquare bd u_xy }
        where
          removeUnitSquare board (x,y) = setCell (x+u_x) (y+u_y) Empty board
          u_xy = unit_xy $ curr_unit gameData
          (u_x, u_y) = curr_unit_xy gameData
          bd = board gameData

placeUnitOnBoard gameData =
    gameData { board = setCell u_x u_y Piece_Pivot (foldl' placeUnitSquare bd u_xy) }
        where
          placeUnitSquare board (x,y) = setCell (x+u_x) (y+u_y) Piece bd
          u_xy = unit_xy $ curr_unit gameData
          (u_x, u_y) = curr_unit_xy gameData
          bd = board gameData

lockUnitOnBoard gameData =
    gameData { board = foldl' lockUnitSquare bd u_xy }
        where
          lockUnitSquare board (x,y) = setCell (x+u_x) (y+u_y) Locked bd
          u_xy = unit_xy $ curr_unit gameData
          (u_x, u_y) = curr_unit_xy gameData
          bd = board gameData

canPlace gameData =
    (List.all cellOK u_xy) && ((List.find ((/=) Empty) (map getUnitCell u_xy)) == Nothing)
        where
          getUnitCell (x,y) = getCell (u_x+x) (u_y+y) bd
          cellOK (x,y) = (u_x + x >= 0) && (u_x + x < w bd) && (u_y + y >= 0) && (u_y + y < h bd)
          u_xy = unit_xy $ curr_unit gameData
          (u_x, u_y) = curr_unit_xy gameData
          bd = board gameData

movePiece MOVE_E gameData = let (x,y) = curr_unit_xy gameData in gameData { curr_unit_xy = (x+1,y) }
movePiece MOVE_W gameData = let (x,y) = curr_unit_xy gameData in gameData { curr_unit_xy = (x-1,y) }
movePiece MOVE_SE gameData = let (x,y) = curr_unit_xy gameData in gameData { curr_unit_xy = getCellByDir x y SE }
movePiece MOVE_SW gameData = let (x,y) = curr_unit_xy gameData in gameData { curr_unit_xy = getCellByDir x y SW }
movePiece MOVE_CLOCKWISE gameData = gameData { curr_unit = rotateBoardUnit (curr_unit gameData) CLOCKWISE (rotate_map gameData) }
movePiece MOVE_COUNTERCLOCKWISE gameData = gameData { curr_unit = rotateBoardUnit (curr_unit gameData) COUNTERCLOCKWISE (rotate_map gameData) }

isHole (Board w h cells) (x,y) =
    if y == h-1 then
        False
    else if (cellValue /= Empty) && (belowCell == Empty) then
             True
         else
             False
                 where
                   cellValue = getCell x y (Board w h cells)
                   belowCell = getCell x (y+1) (Board w h cells)

scoreBoard gameData =
    (countLocked bd) + (countHoles bd) + (countFull bd)
        where
          countLocked (Board w h cells) = List.sum . V.toList $ V.imap cellValue cells
          countHoles (Board w h cells) = (-2) * (List.length $ List.filter (isHole bd) [(x,y) | x <- [0..bw-1],
                                                                                  y <- [0..bh-1]])
          countFull board = 4 * (List.length $ fullRows board)
          cellValue i v = if v /= Locked then 0 else (i `div` bw)
          bd = board gameData
          bw = w bd
          bh = h bd
              
scoreGame gameData =
    gameData { score = (score gameData) + moveScore,
               board = clearFullRows (board gameData),
               ls_old = numFull }
        where
          numFull = List.length $ fullRows (board gameData)
          points = (length (unit_xy $ curr_unit gameData)) + 100 * (1 + numFull) * (numFull `div` 2)
          line_bonus = if (ls_old gameData) > 1 then
                           ((ls_old gameData) - 1) * points `div` 10
                       else
                           0
          moveScore = points + line_bonus

availableMoves gameData =
    filter (canMove gameData) [MOVE_E, MOVE_W, MOVE_SE, MOVE_SW, MOVE_CLOCKWISE, MOVE_COUNTERCLOCKWISE]

canMove gameData move =
    let gameData2 = movePiece move (removeUnitFromBoard gameData) in
    (canPlace gameData2) && (List.notElem (board $ placeUnitOnBoard gameData2) (board_history gameData2))
    
applyMove gameData move =
    let gameData2 = 
            if canMove gameData move then
                placeUnitOnBoard . (movePiece move) $ removeUnitFromBoard gameData 
            else
                lockUnitOnBoard gameData in
    let gameData3 = gameData2 { board_history = (board gameData2) : (board_history gameData2) } in
    let gameData4 = scoreGame gameData3 in
    gameData4 { move_list = move : move_list gameData4 }

tryApplyMove gameData move =
    (best_score, move : best_moves, b : best_board)
        where
          b = applyMove gameData move
          (best_score, best_moves, best_board) = computeMoveList b

computeMoveList gameData =
    if null am then
        (score lockAndScore, [MOVE_SE], [lockAndScore])
    else
        List.maximumBy compareScores ((map (tryApplyMove gameData) am) ++ canLock)
            where
              am = availableMoves gameData
              compareScores (am1,_,_) (am2,_,_) = compare am1 am2
              lockAndScore = scoreGame $ lockUnitOnBoard gameData
              lockingMoves = filter (\m -> notElem m am) [MOVE_E, MOVE_W, MOVE_SE, MOVE_SW]
              canLock =
                  if null lockingMoves then
                      []
                  else
                      [(score lockAndScore, [head lockingMoves], [lockAndScore])]

lockable gameData (x,y,(_,_,rot)) =
    (canPlace pieceAtXY) && lockingMovesAvailable
        where
          pieceAtXY = (removeUnitFromBoard gameData) { curr_unit = rot,
                                                       curr_unit_xy = (x,y) }
          am = availableMoves (placeUnitOnBoard pieceAtXY)
          lockingMovesAvailable = not . null $ filter (\m -> notElem m am) [MOVE_E, MOVE_W, MOVE_SE, MOVE_SW]
          rotateNTimes u n = foldl' rotateOnce u [1..n]
          rotateOnce u _ = rotateBoardUnit u CLOCKWISE (rotate_map gameData)

unitRotations gameData =
    foldl' addUniqueShape [] $ map rotateNTimes [(d,n) | n <- [0..5], d <- [CLOCKWISE, COUNTERCLOCKWISE]]
        where
          u = curr_unit gameData
          rotateNTimes (d,n) = (d,n,foldl' (rotateOnce d) u [1..n])
          rotateOnce d u _ = rotateBoardUnit u d (rotate_map gameData)
          addUniqueShape shapes shape =
              if uniqueShape shape shapes then
                  shape : shapes
              else
                  shapes
          uniqueShape shape shapes =
              (find (shapeMatches shape) shapes) == Nothing
          shapeMatches (_,_,s1) (_,_,s2) = s1 == s2

makeLockLocationList gameData =
    nub $ filter (lockable gameData) [(x,y,rots) | x <- [0..bw-1], y <- [0..bh-1], rots <- unitRotations gameData]
        where
          bd = board gameData
          bh = h bd
          bw = w bd

initMoveMap gameData =
    Map.fromList $ map createLockedMove (makeLockLocationList gameData)
        where
          createLockedMove (x,y,(d,n,shape)) = ((x,y),([lockDir (x,y) shape],d,n,lockShape (x,y) shape,shape))
          lockShape (x,y) shape = scoreBoard . lockUnitOnBoard $ gameData { curr_unit = shape, curr_unit_xy = (x,y) }
          lockDir (x,y) shape = head $ lockingMoves (removeUnitFromBoard $ gameData { curr_unit = shape, curr_unit_xy = (x,y) })
          lockingMoves gameData = filter (\m -> notElem m (availableMoves gameData)) [MOVE_E, MOVE_W, MOVE_SE, MOVE_SW]

updateMoveMap gameData mm =
    Map.foldlWithKey' addConnectingMoves mm mm
        where
          addConnectingMoves m2 (x,y) move_entry =
              foldl' addToMap m2 (makeMoveList gameData (x,y) move_entry)
          addToMap m3 ((x,y),(dirList,d,n,score,shape)) =
              Map.insertWith pickBestScore (x,y) (dirList,d,n,score,shape) m3
          pickBestScore (dl1,d1,n1,score1,shape1) (dl2,d2,n2,score2,shape2) =
              if score1 > score2 then (dl1,d1,n1,score1,shape1) else (dl2,d2,n2,score2,shape2)

makeMoveList gameData (x,y) (dirList,d,n,score,shape) =
    filter isValidMove (map makeMove [(NE,MOVE_SW),(NW,MOVE_SE),(E,MOVE_W),(W,MOVE_E)])
        where
          makeMove (backDir,forwardDir) = ((getCellByDir x y backDir),(forwardDir:dirList,d,n,score,shape))
          isValidMove ((x,y),(_,_,_,_,shape)) =
              canPlace $ (removeUnitFromBoard gameData) { curr_unit = shape, curr_unit_xy = (x,y) }
              
completeMoveMap gameData oldMap =
    if (old_size == new_size) && (old_score == new_score) then
        oldMap
    else
        completeMoveMap gameData newMap
            where
              old_size = Map.size oldMap
              old_score = score_map oldMap
              newMap = updateMoveMap gameData oldMap
              new_size = Map.size newMap
              new_score = score_map newMap
              score_map m = Map.foldl' add_score 0 m
              add_score s (_,_,_,s2,_) = s + s2

createMoveMap gameData =
    completeMoveMap gameData (initMoveMap gameData)

applyMoveList gameData moveMap =
    foldl' applyMove gameData (moveDirs $ moveMap Map.! (curr_unit_xy gameData))
        where
          moveDirs (dirs, d, n, _, _) = (initialRotate d n) ++ dirs
          initialRotate CLOCKWISE n = replicate n MOVE_CLOCKWISE
          initialRotate COUNTERCLOCKWISE n = replicate n MOVE_COUNTERCLOCKWISE

runGame gameData =
    if canStartNextPiece nextRound then
        runGame $ startNextPiece nextRound
    else
        nextRound
            where
              nextRound = applyMoveList gameData (createMoveMap gameData)
              canStartNextPiece g = canPlace $ startNextPiece g
              startNextPiece g = g { curr_unit = head $ remaining_units g,
                                     curr_unit_xy = computeUnitStart (bw g) (head $ remaining_units g),
                                     remaining_units = tail $ remaining_units g,
                                     full_board_history = (board_history g) ++ (full_board_history g),
                                     board_history = [] }
              bw g = w $ board g

    
getBoard gameData [] = []
getBoard gameData (m:ml) =
    board nextGame : getBoard nextGame ml
        where
          nextGame = applyMove gameData m