import Sudoku
import Solver

b = boardS "8 3 9 6 4 2 7 1 5 4 5 0 7 1 8 6 3 9 1 7 6 5 3 9 4 8 2 6 0 3 4 8 7 5 9 1 5 9 8 1 6 3 2 4 7 7 1 4 9 2 5 8 6 3 9 4 7 8 5 1 3 2 6 3 8 1 2 7 6 9 5 4 2 6 5 3 9 4 1 7 8"
p = cells . slots $  b

e = boardS ""
o = implement e ((0,0), 1)
s = slots e
