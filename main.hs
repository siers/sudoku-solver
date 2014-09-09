import System.Environment
import Control.Arrow
import Sudoku
import Solver

representLog :: ([Point Int], SBoard) -> String
representLog (p, b) = show p ++ "\n" ++ showB b ++ "\n"

-- ./main < sudoku
main = mapM (putStrLn . representLog) . solve . (,) [] . boardS =<< getContents
