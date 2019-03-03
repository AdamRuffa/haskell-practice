module Leetcode.Nqueens where

import Data.Bool
import Control.Lens

data Board = Board { _board  :: [[Char]]
                   , diag    :: [Bool]
                   , row     :: [Bool]
                   , col     :: [Bool]
                   , nQueens :: Int
                   , mQueens :: Int
                   , size    :: Int
                   } deriving (Show, Eq)

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
diagonal size (row, col) = if row < col
                            then diagonal size (0, col - row)
                            else diagonal size (row - col, 0)

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
         | nQueens == size = False
         | otherwise = True
         where base  = Board _board diag r c nQueens mQueens size
               co    = (row, col)