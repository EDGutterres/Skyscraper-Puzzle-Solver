import Data.List

{-
Skyscraper puzzle solver

example input: 
    solve ([2,2,1],[1,2,2],[3,1,2],[2,1,3])
solution:
    [[1,2,3],[3,1,2],[2,3,1]]
diagram of above puzzle:
    | 2 | 2 | 1 |
 ___|___|___|___|___
  3 | 1 | 2 | 3 | 1 
 ___|___|___|___|___ 
  1 | 3 | 1 | 2 | 2 
 ___|___|___|___|___ 
  2 | 2 | 3 | 1 | 2 
 ___|___|___|___|___ 
    | 2 | 1 | 3 | 
    |   |   |   |
In general, it will take the form:
    | a | b | c |
 ___|___|___|___|___
  l | 1 | 2 | 3 | d 
 ___|___|___|___|___ 
  k | 4 | 5 | 6 | e 
 ___|___|___|___|___ 
  j | 7 | 8 | 9 | f 
 ___|___|___|___|___ 
    | i | h | g | 
    |   |   |   |
input: 
    solve ([a,b,c],[d,e,f],[g,h,i],[j,k,l])
solution:
    [[1,2,3],[4,5,6],[7,8,9]]
 -}

---------------------------------------------------------------------
---------------------------------------------------------------------
-- PRIMARY FUNCTION -------------------------------------------------

{-
 - This is the primary function - takes a tuple of all 4 sides and generates a solution.
 - If a solution does not exist, this function will throw an exception.
 -}
solve conds@(top,_,_,_) = 
  let n = (length top)
  in head (filter (isBoardValid conds) (genPrunedBoards n))



---------------------------------------------------------------------
---------------------------------------------------------------------
-- FUNCTIONS TO CHECK IF A BOARD IS A SOLUTION ----------------------

{-
 - Helper to check if a given, filled board is a valid solution
 -}
isBoardValid :: Integral a => ([a],[a],[a],[a]) -> [[a]] -> Bool
isBoardValid (top,right,bottom,left) board = topIsValid && rightIsValid && bottomIsValid && leftIsValid 
    where
        ns = [0..(length top)-1]
        topIsValid    = (and [dirValid (top!!n) (getCol n board)                        | n <- ns])
        rightIsValid  = (and [dirValid (right!!n) (reverse (board!!n))                  | n <- ns])
        bottomIsValid = (and [dirValid ((reverse bottom)!!n) (reverse (getCol n board)) | n <- ns])
        leftIsValid   = (and [dirValid ((reverse left)!!n) (board!!n)                   | n <- ns])


{-
 - Given a condition and a list, it checks whether the puzzle property is true for that list.
 -
 - E.g. dirValid 2 [3,2,1,4] is true, because only 2 squares can be "seen" from the list 
 - in this direction.
 - 
 - E.g. dirValid 3 [4,1,2,3] is false, because only 1 square can be seen in this direction.
 -}
dirValid _ [] = True
dirValid n [x,y]
  | x == y = False -- boards with two of the same element are invalid
  | n > 2  = False
  | n == 1 = x > y
  | otherwise = x < y

dirValid n lst@(x:y:xs)
  | hasDuplicates lst = False
  | n == 1 = (x > y) && (dirValid 1 (x:xs))
  | x < y = dirValid (n - 1) (y:xs)
  | x > y = dirValid n (x:xs)



---------------------------------------------------------------------
---------------------------------------------------------------------
-- FUNCTIONS TO GENERATE A PRUNED LIST OF N x N BOARDS---------------

{-
 - Helper to generate the pruned list of all valid boards of size n x n
 -}
genPrunedBoards :: Integral a => a -> [[[a]]] -- note: a single board has type [[a]]
genPrunedBoards n = pruneCols (genBoards n n [])

{-
 - Helper to getPrunedBoards - filters a list for only boards with valid columns.
 -}
pruneCols :: Integral a => [[[a]]] -> [[[a]]]
pruneCols boards = 
  filter noDupCols boards

{-
 - Helper to pruneCols - returns true if all columns of a board are valid
 
 e.g. this board:
              | a | b | c |
           ___|___|___|___|___
            l | 1 | 2 | 3 | d 
           ___|___|___|___|___ 
            k | 2 | 3 | 1 | e 
           ___|___|___|___|___ 
            j | 3 | 1 | 2 | f 
           ___|___|___|___|___ 
              | i | h | g | 
              |   |   |   |
    is valid, since a number is not repeated in a column, whereas:
              | a | b | c |
           ___|___|___|___|___
            l | 1 | 2 | 3 | d 
           ___|___|___|___|___ 
            k | 2 | 3 | 1 | e 
           ___|___|___|___|___ 
            j | 1 | 3 | 2 | f 
           ___|___|___|___|___ 
              | i | h | g | 
              |   |   |   |
    is not, since 3 is repeated in column b and 1 is repeated in column a.
 -}
noDupCols :: Integral a => [[a]] -> Bool
noDupCols board = 
  not (or (map hasDuplicates (getAllCols board)))


{-
 - generates a list of possible n x n boards, completely pruned row-wise but
 - only partially pruned column-wise.
 -}
genBoards :: Integral a => a -> a -> [[[a]]] -> [[[a]]]
genBoards n 0 acc = acc
genBoards n c [] = genBoards n (c-1) (genChildBoards n [])
genBoards n c acc = 
    foldr (++) [] (map (genChildBoards n) (genBoards n (c-1) acc))

{-
 - Helper to genBoards, generates a list of all child boards for a given board,
 - partially pruned.
 -}
genChildBoards :: Integral a => a -> [[a]] -> [[[a]]]
genChildBoards n board = 
    [x:board | x <- permutations [1..n], not (x `elem` board)]



---------------------------------------------------------------------
---------------------------------------------------------------------
-- MISC HELPER FUNCTIONS --------------------------------------------

{-
 - helper to check if a list has duplicates
 -}
hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates []  = False
hasDuplicates (x:xs) = (x `elem` xs) || (hasDuplicates xs)


{-
 - Helper to get a list of all columns from a given board
 -}
getAllCols board = [(getCol n board) | n <- [0..((length board)-1)]]


{-
 - Helper to get a single column from a board as a list of int
 -}
getCol n [] = []
getCol n board = 
    ((head(board))!!n):(getCol n (tail(board)))




