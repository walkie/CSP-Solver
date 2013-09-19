{-# LANGUAGE PatternGuards #-}

-- | A small and simple CSP solver.
module CSP where

import Data.Function (on)
import Data.List     (deleteBy,findIndex,intersect,sortBy)


-- * Variables

-- | A variable has a name and a list of possible values (its domain).
type Variable n a = (n,[a])

-- | The name of a variable.
name :: Variable n a -> n
name = fst

-- | The domain of possible values of a variable.
domain :: Variable n a -> [a]
domain = snd

-- | A variable's order is the length of its domain.
order :: Variable n a -> Int
order = length . domain

-- | 'True' if the variables have the same name.
sameName :: Eq n => Variable n a -> Variable n a -> Bool
sameName = (==) `on` name

-- | Is the variable undefined?
--   'True' if its domain is empty.
undef :: Variable n a -> Bool
undef = null . domain

-- | Is the variable defined?
--   'True' if its domain is non-empty.
def :: Variable n a -> Bool
def = not . undef

-- | Is the variable assigned?
--   'True' if its domain contains exactly one element.
assigned :: Variable n a -> Bool
assigned = (==1) . order

-- | Is the variable unassigned?
--   'True' if it is not assigned.
unassigned :: Variable n a -> Bool
unassigned = not . assigned

-- | Sort a list of variables by order,
--   from most constrained (smallest order) to least.
triage :: [Variable n a] -> [Variable n a]
triage = sortBy (compare `on` order)


-- * Constraints

-- | A constraint is a function from a list of variables
--   to a boolean value indicating whether the constraint is satisfied.
type Constraint n a = [Variable n a] -> Bool

-- | Is the problem consistent?
--   'True' if all constraints are satisfied.
check :: [Constraint n a] -> [Variable n a] -> Bool
check cs vs = all ($ vs) cs

-- | Is the problem solved?
--   'True' if all constraints are satisfied and if all variables are assigned.
solved :: [Constraint n a] -> [Variable n a] -> Bool
solved cs vs = all assigned vs && check cs vs

-- | Solve a CSP problem.
--   Returns all solutions for given a list of constraints and variables.
solve :: Eq n => [Constraint n a] -> [Variable n a] -> [[Variable n a]]
solve cs vs | any undef pre = []
            | Nothing <- next = [pre]
            | Just i  <- next = let (av,(n,d):uv) = splitAt i pre
                                in concat [solve cs ((n,[a]):av ++ uv) | a <- d]
  where pre  = triage (prune cs vs)
        next = findIndex unassigned pre

-- | Prune the search space by removing from the domain of each variable any
--   value which would immediately violate a constraint.
prune :: Eq n => [Constraint n a] -> [Variable n a] -> [Variable n a]
prune cs vs = foldl f vs vs
  where f vs v@(n,d) = let vs' = deleteBy sameName v vs
                       in (n, [a | a <- d, check cs ((n, [a]) : vs')]) : vs'

-- | An example constraint, indicating that two variables, given by name, should be equal.
eq :: (Eq n, Eq a) => n -> n -> Constraint n a
eq a b vs = (not . null) (da `intersect` db)
  where Just da = lookup a vs
        Just db = lookup b vs


-- * Example: N-Queens Problem

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
allSafe []     = True
allSafe (q:qs) = all (safe q) qs && allSafe qs

-- | Find all solutions to the n-queens problem, for a given value of end.
--   'head' can be used to get a single solution only, for example,
--   @head (queens 20)@.
queens :: Int -> [[Queen]]
queens n = solve [allSafe] qs
  where qs = [(r,[1..n]) | r <- [1..n]]
