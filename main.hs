import System.Environment
import Control.Arrow
import Sudoku
import Solver

(>$>) = flip fmap

representSnoop :: ([Point Int], SBoard) -> String
representSnoop (p, b) = show p ++ "\n" ++ showB b ++ "\n"

-- ./main < sudoku
main = getContents >$> boardS >>= mapM (putStrLn . representSnoop) . solve . (,) []
-- main = getContents >$> boardS >>= mapM (putStrLn . (++"\n") . showB) . convergerate trivializer
