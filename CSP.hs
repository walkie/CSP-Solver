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

-- | Solve a CSP problem and return the first solution, if any.
solve1 :: Eq n => [Constraint n a] -> [Variable n a] -> Maybe [Variable n a]
solve1 cs vs | (s:_) <- solve cs vs = Just s
             | otherwise            = Nothing

-- | Prune the search space by removing from the domain of each variable any
--   value which would immediately violate a constraint.
prune :: Eq n => [Constraint n a] -> [Variable n a] -> [Variable n a]
prune cs vs = foldl f vs vs
  where f vs v@(n,d) = let vs' = deleteBy sameName v vs
                       in (n, [a | a <- d, check cs ((n, [a]) : vs')]) : vs'

-- | An example constraint, indicating that two variables, given by name,
--   should be equal.
eq :: (Eq n, Eq a) => n -> n -> Constraint n a
eq a b vs = (not . null) (da `intersect` db)
  where Just da = lookup a vs
        Just db = lookup b vs

-- | Given a binary predicate, construct a constraint that it is pairwise
--   satisfied for all variables.
pairwise :: (Variable n a -> Variable n a -> Bool) -> Constraint n a
pairwise f []     = True
pairwise f (v:vs) = all (f v) vs && pairwise f vs
