module Solver where

import Data.Maybe
import Data.List
import Data.List.Split
import Control.Arrow
import Sudoku

frequency :: Ord a => Line a -> Line (Int, a)
frequency = map (length &&& head) . group . sort

-- Expects that the Board is a Line of possibilities within same restriction group.
uniq :: Ord a => Board a -> Line a
uniq = map snd . filter ((==1) . fst) . frequency . concat

-- Ord type restriction could is not needed for functionality, just for laziness.
-- Checks for unique numbers in lines of h/v/s boards, retrieves them as Target Int.
trivialities :: Ord a => LSlots a -> [Point a]
trivialities = concatMap (targets) . align
    where
        align           = zip =<< simples
        simples         = map (uniq . map snd)
        sample          = find (not . null . snd) . return
        allowed uniques = map (second (intersect uniques))
        targets         = catMaybes . map (fmap (second head) . sample) . uncurry allowed

trivialized :: SBoard -> [Point Int]
trivialized b = concatMap trivialities . streams $ possibs
    where
        possibs = cells . unify . takens $ b

logTrivialize :: ([Point Int], SBoard) -> ([Point Int], SBoard)
logTrivialize (_, b) =
    if length (trivialized b) /= 0
    then (nub . trivialized $ b, implement b . head . trivialized $ b)
    else ([], b)

trivialize :: SBoard -> SBoard
trivialize = snd . curry logTrivialize undefined

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

convergerate :: Eq a => (a -> a) -> a -> [a]
convergerate f i = takeWhile (/= (converge f i)) $ iterate f i

-- Converge and preserve iterations.
solve = convergerate logTrivialize
