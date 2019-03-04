module Leetcode.Nqueens where

import Data.Bool
import Control.Lens

data Board = Board { _board   :: [[Char]]
                   , _diag    :: [Bool]
                   , _diag'   :: [Bool]
                   , _row     :: [Bool]
                   , _col     :: [Bool]
                   , _size    :: Int
                   } deriving (Show, Eq)
                 
-- entrypoint function returns list of board representations;
-- Usage: nQueens 5 3 e.g. on a 5x5 board search for permutations of 3 valid queens
nQueens  :: Int -> Int -> [[[Char]]]
nQueens 0 _ = [[[]]]
nQueens sizeOfBoard numQueens = map _board $ nQueens' sizeOfBoard numQueens
nQueens' :: Int -> Int -> [Board]  
nQueens' size num | size <= 0 || num < 0   = []
                  | num == 0               = getBoard $ emptyBoard size 0
                  | otherwise              = qround 0 num $ getBoard $ emptyBoard size num

-- recursive permutation builder; starts with base permutations and uses
-- those as inputs to the next round of permutation generation                 
qround :: Int -> Int -> [Board] -> [Board]
qround round rounds boards | round < 0 || round >= rounds = boards
                           | otherwise = (qround (round + 1) rounds) (qround' round boards)
qround' :: Int -> [Board] -> [Board]
qround' _ [] = []
qround' round boards = (foldr (++) []) . (foldr (++) []) $
                       map (\co -> map (\b -> getBoard $ placeQ (Just b) co) 
                                       boards
                           ) $ [(r,c) | r <- perms, c <- perms]
                     where perms = [0..(_size $ boards!!0)-1]

-- initalizers
emptyBools :: Int -> [Bool]
emptyBools size = replicate size False
emptyBoard :: Int -> Int -> Maybe Board
emptyBoard size wantedQueens =   Just $ Board (map (replicate size) $ replicate size '.') 
                                              diagonals diagonals
                                              laterals laterals
                                              size
                             where diagonals = emptyBools (size * 2 - 1)
                                   laterals  = emptyBools size
                             

-- calculations for diagonal uniqueness
diagonal :: Int -> (Int, Int) -> Int
diagonal size (0, col) = size - 1 + col
diagonal size (row, 0) = size - 1 - row
diagonal size (row, col) | row < col = diagonal size (0, col - row)
                         | otherwise = diagonal size (row - col, 0)
diagonal' :: Int -> (Int, Int) -> Int
diagonal' _ (row, col) = row + col

-- flip a row/col/diagonal state from valid (e.g. can place a Queen)
-- to invalid, if a valid row/col/diag and not already invalid
placeBool :: Int -> [Bool] -> [Bool]
placeBool index list = list & element index .~ not (list!!index)
placeChar :: (Int, Int) -> [[Char]] -> [[Char]]
placeChar (row, col) board 
 | board!!row!!col == 'Q' = board
 | otherwise = (take row board)
               ++ [(board!!row) & element col .~ 'Q'] ++
               (drop (row + 1) board)
                                         
-- conversion from Maybe to [] monad
getBoard :: Maybe Board -> [Board]
getBoard board | board == Nothing = []
               | otherwise        = (\(Just b) -> [b]) board
               
-- put a queen on a board if possible, otherwise fail this permutation
placeQ  :: Maybe Board -> (Int, Int) -> Maybe Board
placeQ  board (row, col) = board >>= (\board -> placeQ' board (row, col))
placeQ' :: Board -> (Int, Int) -> Maybe Board
placeQ' base@(Board _board diag diag' r c size) co@(row, col) 
        | not $ canPlace base co = Nothing
        | otherwise              = Just $ Board (placeChar co _board)
                                                (placeBool (diagonal size co) diag)
                                                (placeBool (diagonal' size co) diag')
                                                (placeBool row r)
                                                (placeBool col c)
                                                size

-- checker to see if we can place a queen on given coordinates of a Board state
canPlace :: Board -> (Int, Int) -> Bool
canPlace base@(Board _board diag diag' r c size) co@(row, col)
 = not $    diag!!(diagonal size co) || diag'!!(diagonal' size co)
         || r!!row || c!!col