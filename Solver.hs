module Solver where

import Data.List.Split
import Sudoku

type Slots  = [[[Int]]]
data Solver = Solver Board Slots

freeSlots = splitEvery 9 $ splitEvery 9 $ take 81 $ repeat [1..9]

solve = id
