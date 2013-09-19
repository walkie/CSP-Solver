-- | Example: N-Queens Problem
module NQueens where

import CSP


type Row = Int
type Col = Int

-- | A variable in the n-queens problem, representing a single queen.
--   The variable's name is the queen's row, while its column is the value to
--   to be assigned.
type Queen = Variable Row Col

-- | 'True' if the two queens can attack each other.
--   That is, if they are on the same row, column, or diagonal.
canAttack :: Queen -> Queen -> Bool
canAttack (r,[c]) (s,[d]) = r == s || c == d || abs (r-s) == abs (c-d)
canAttack _ _ = False

-- | 'True' if the two queens cannot attack each other.
safe :: Queen -> Queen -> Bool
safe = (not .) . canAttack

-- | A constraint indicating that all queens are pairwise safe.
allSafe :: [Queen] -> Bool
allSafe = pairwise safe

-- | Find all solutions to the n-queens problem, for a given value of end.
--   'head' can be used to get a single solution only, for example,
--   @head (queens 20)@.
queens :: Int -> [[Queen]]
queens n = solve [allSafe] qs
  where qs = [(r,[1..n]) | r <- [1..n]]
