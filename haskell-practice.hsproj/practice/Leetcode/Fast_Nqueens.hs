module Leetcode.Fast_Nqueens where

import Data.Bool 
import Data.List
import Data.HashMap.Strict hiding (map)
import Data.Hashable
import Control.Lens

data Board = Board { _board   :: [[Char]]
                   , _diag    :: [Bool]
                   , _diag'   :: [Bool]
                   , _row     :: [Bool]
                   , _col     :: [Bool]
                   , _size    :: Int
                   } deriving (Show, Eq)
instance Hashable Board where
  hashWithSalt s board@(Board a b c d e f) = 
   s + (hash a) * 31 + (hash b) * 31 + (hash c) * 31 
     + (hash d) * 31 + (hash e) * 31 + (hash f) * 31
                 
-- entrypoint function returns list of board representations;
-- Usage: nQueens 5 3 e.g. on a 5x5 board search for permutations of 3 valid queens
nQueens  :: Int -> Int -> [[[Char]]]
nQueens sizeOfBoard numQueens = map _board $ nQueens' sizeOfBoard numQueens
nQueens' :: Int -> Int -> [Board]  
nQueens' size num 
 | size <= 0 || num < 0 = []
 | num == 0             = emptyBoard size
 | otherwise            = qround 0 num (emptyBoard size) size

-- recursive permutation builder; starts with base permutations and uses
-- those as inputs to the next round of permutation generation                 
qround :: Int -> Int -> [Board] -> Int -> [Board]
qround round rounds boards size
 | round < 0 || round >= rounds = boards
 | otherwise = (qround (round + 1) rounds) (qround' round boards size) $ size
qround' :: Int -> [Board] -> Int -> [Board]
qround' _ [] _ = []
qround' round boards size = 
 concat . concat
 $ map (\co -> map (\b -> placeQ [b] co) boards) 
 $ [(r,c) | r <- perms, c <- perms]
 where perms = [0..(size-1)]

-- initalizers
emptyBools :: Int -> [Bool]
emptyBools size = replicate size False
emptyBoard :: Int -> [Board]
emptyBoard size =   
 [Board (map (replicate size) $ replicate size '.') 
        diagonals diagonals laterals laterals size]
 where diagonals = emptyBools (size * 2 - 1)
       laterals  = emptyBools size
                             
-- calculations for diagonal uniqueness
diagonal :: Int -> (Int, Int) -> Int
diagonal size (0, col) = size - 1 + col
diagonal size (row, 0) = size - 1 - row
diagonal size (row, col) 
 | row < col = diagonal size (0, col - row)
 | otherwise = diagonal size (row - col, 0)
diagonal' :: (Int, Int) -> Int
diagonal' (row, col) = row + col

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
                              
-- put a queen on a board if possible, otherwise fail this permutation
placeQ  :: [Board] -> (Int, Int) -> [Board]
placeQ  board (row, col) = board >>= (\board -> placeQ' board (row, col))
placeQ' :: Board -> (Int, Int) -> [Board]
placeQ' base@(Board _board diag diag' r c size) co@(row, col) 
 | not $ canPlace base co = []
 | otherwise = [Board (placeChar co _board)
                      (placeBool (diagonal size co) diag)
                      (placeBool (diagonal' co) diag')
                      (placeBool row r) (placeBool col c) size]

-- checker to see if we can place a queen on given coordinates of a Board state
canPlace :: Board -> (Int, Int) -> Bool
canPlace base@(Board _board diag diag' r c size) co@(row, col)
 = not $    diag!!(diagonal size co) || diag'!!(diagonal' co)
         || r!!row || c!!col
         