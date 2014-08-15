import System.Environment
import Control.Arrow
import Sudoku
import Solver

(>$>) = flip fmap

representLog :: ([Point Int], SBoard) -> String
representLog (p, b) = show p ++ "\n" ++ showB b ++ "\n"

-- ./main < sudoku
main = getContents >$> boardS >>= mapM (putStrLn . representLog) . solve . (,) []
