module Leetcode.Nqueens where

import Data.Bool
import Control.Lens

data Board = Board { _board  :: [[Char]]
                   , _diag    :: [Bool]
                   , _row     :: [Bool]
                   , _col     :: [Bool]
                   , _nQueens :: Int
                   , _mQueens :: Int
                   , _size    :: Int
                   } deriving (Show, Eq)
                 
nQueens :: Int -> Int -> [[[Char]]]
nQueens 0 _ = [[[]]]
nQueens sizeOfBoard numQueens = map _board $ nQueens' sizeOfBoard numQueens


nQueens' :: Int -> Int -> [Board]  
nQueens' size num | size <= 0 = []
                  | num < 0   = []
                  | num == 0  = getBoard $ emptyBoard size 0
                  | otherwise = qround 0 num $ getBoard $ emptyBoard size num
                 

qround :: Int -> Int -> [Board] -> [Board]
qround round rounds boards | round < 0       = boards
                           | round >= rounds = boards
                           | otherwise       = (qround (round + 1) rounds) (qround' round boards)
qround' :: Int -> [Board] -> [Board]
qround' _ [] = []
qround' round boards = 
 (foldr (++) []) . (foldr (++) []) $
 map (\co -> map (\b -> getBoard $ placeQ (Just b) co) 
                 boards
     ) $ allPositions (_size $ boards!!0)

emptyBools :: Int -> [Bool]
emptyBools size = replicate size False

emptyBoardString :: Int -> [[Char]]
emptyBoardString size = map (replicate size) $ replicate size '.'

emptyBoard :: Int -> Int -> Maybe Board
emptyBoard size wantedQueens =   Just $ Board (emptyBoardString size) 
                                              (emptyBools (size * 2 - 1)) 
                                              (emptyBools size) 
                                              (emptyBools size)
                                              0 wantedQueens size

diagonal :: Int -> (Int, Int) -> Int
diagonal size (0, col) = size - 1 + col
diagonal size (row, 0) = size - 1 - row
diagonal size (row, col) | row < col = diagonal size (0, col - row)
                         | otherwise = diagonal size (row - col, 0)

placeBool :: Int -> [Bool] -> [Bool]
placeBool index list | index < 0 = list
                     | index >= (length list) = list
                     | list!!index == True = list
                     | otherwise = list & element index .~ True
                     
placeChar :: (Int, Int) -> [[Char]] -> [[Char]]
placeChar (row, col) board | row < 0 = board
                           | col < 0 = board
                           | row >= length board = board
                           | col >= length (board!!0) = board
                           | board!!row!!col == 'Q' = board
                           | otherwise = (take row board)
                                         ++ [(board!!row) & element col .~ 'Q'] ++
                                         (drop (row + 1) board)
                                         
getBoard :: Maybe Board -> [Board]
getBoard board | board == Nothing = []
               | otherwise        = (\(Just b) -> [b]) board
                                         

allPositions :: Int -> [(Int, Int)]
allPositions size | size < 0  =  []
                  | otherwise =  foldr (++) []
                                       (map (\row -> map (\col -> (row, col)) 
                                                         [0..(size - 1)])
                                            [0..(size - 1)])

placeQ :: Maybe Board -> (Int, Int) -> Maybe Board
placeQ board (row, col) = board >>= (\board -> placeQ' board (row, col))
placeQ' :: Board -> (Int, Int) -> Maybe Board
placeQ' (Board _board diag r c nQueens mQueens size) (row, col) 
        | not $ canPlace base co = Nothing
        | otherwise              = Just $ Board (placeChar co _board)
                                                (placeBool (diagonal size co) diag)
                                                (placeBool row r)
                                                (placeBool col c)
                                                (nQueens + 1) mQueens size
        where base  = Board _board diag r c nQueens mQueens size
              co    = (row, col)

canPlace :: Board -> (Int, Int) -> Bool
canPlace (Board _board diag r c nQueens mQueens size) (row, col)
         | row < 0 = False
         | col < 0 = False
         | row >= size = False
         | col >= size = False
         | diag!!(diagonal size co) = False
         | r!!row = False
         | c!!col = False
         | nQueens == mQueens = False
         | otherwise = True
         where base  = Board _board diag r c nQueens mQueens size
               co    = (row, col)