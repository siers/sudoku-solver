module Solver where

import Data.Maybe
import Data.List
import Data.List.Split
import Control.Arrow
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Logic.Class
import Sudoku

frequency :: Ord a => Line a -> Line (Int, a)
frequency = map (length &&& head) . group . sort

-- Expects that the Board is a Line of possibilities within same restriction group.
uniq :: Ord a => Board a -> Line a
uniq = map snd . filter ((== 1) . fst) . frequency . concat

-- Ord type restriction could is not needed for functionality, just for laziness.
-- Checks for unique numbers in lines of one of h/v/s board, retrieves them as Target Int.
trivialities :: Ord a => LSlots a -> [Point a]
trivialities = concatMap targets . align
    where
        align           = zip =<< simples
        simples         = map (uniq . map snd)
        sample          = find (not . null . snd) . return
        allowed uniques = map (second (intersect uniques))
        targets         = catMaybes . map (fmap (second head) . sample) . uncurry allowed

{- It returns a list of numbers that could be the real ones.
 - It should go like [[Point] ...] for Int=1, [[Point, Point] ...] for Int=2.
 - For Int=1, the numbers ARE not only could be the real ones.
 -
 - You can acutually comment out the line with `choices 1' and it will be much slower, but still work.
 -}
choices :: Int -> SBoard -> [[Point Int]]
choices 1 = map return . nub . concatMap trivialities . streams . cells . slots
choices x = filter ((== x) . length) . concat . mapB downpointer . cells . slots
    where downpointer (p, l) = zip (repeat p) l

deductions :: MonadLogic m => SBoard -> m SPoint
deductions = validity =<< body
    where
        offer        = msum . fmap return . concat . take 1
        body         = offer . filter (not . null) . concat . flip map [1..9] . flip choices
        invalid      = not.null . filter empty . concat . (flip zipB =<< slots)
        empty (n, s) = n /= 0 && s /= []
        validity choices board =
            if invalid board
            then mzero
            else choices

guessOnce :: (MonadLogic m, Functor m) => SBoard -> m SBoard
guessOnce = deductions >>= flip (fmap . implement)

guess :: (MonadLogic m, Functor m) => SBoard -> m SBoard
guess b = guessOnce b >>- (\b -> if unfinished b then guess b else return b)
    where unfinished = (0 `elem`) . concat

solve = observe . guess
