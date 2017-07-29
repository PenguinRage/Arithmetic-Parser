-- @author Ian Cleasby --------------------------------------------------

-------------------------------------------------------------------------
-- Setup-----------------------------------------------------------------
-------------------------------------------------------------------------
--
-- Module used for testing
module Assignment1
    (Var, Env, Query(S_var,S_add,S_sub,S_neg),
    findVars,
    transform,
    simplify
    ) where 

import Data.List

-- Types for Var and Environment
type Var = [Char]
-- I originally used double to cover all numbers, 
-- it worked on my version of ghc, 
-- however not on ucpu1 so changed back to Int
type Env = [(Var, Integer)]
-- type Env = [(Var, Double)] 
-- Within data Query is the "expected" syntax
data Query
    = S_var Var
  | S_neg Query
  | S_sub Query Query
  | S_add Query Query
  deriving(Show)

-- In the instance below we are setting cases for equality
-- this is used to determine when to stop simplifying in part 3
instance Eq Query where
    S_var a == S_var b = a == b
    S_neg a == S_neg b = a == b
     -- the relations in add and subtract can meet equality,
     -- as we don't care about the values but the expression.
    S_add a b == S_add c d = (a == c && b == d) || (a == d && b == c)
    S_sub a b == S_sub c d = (a == c && b == d) || (a == d && b == c)
    -- else false
    _ == _ = False

-------------------------------------------------------------------------
-- Question 1------------------------------------------------------------
-------------------------------------------------------------------------
-- findVars is given a query & returns a list with Chars
-- The nub function removes duplicate elements, 
-- grouped by list returned from findAllVars. 
-- In particular, it keeps only the first occurrence of each element.
-- Where the rules of findAllVars returns the variables 
-- from the rule set below.
--
-- Rule Set:
-- S_var: returns variable
-- S_sub q1 q2: call recursion on q1 appending q2
-- S_add: same as S_sub
-- S_neg: call recursion on q

findVars :: Query -> [Var]
findVars q = nub $ findAllVars q
    where findAllVars (S_var v) = [v]
          findAllVars (S_sub q1 q2) = findAllVars q1 ++ findAllVars q2
          findAllVars (S_add q1 q2) = findAllVars q1 ++ findAllVars q2
          findAllVars (S_neg q) = findAllVars q


-------------------------------------------------------------------------
-- Question 2 -----------------------------------------------------------
-------------------------------------------------------------------------
-- transform is given a query & then an enviroment returning a double
-- Here is the rule set for the following code:
-- 
-- Rule Set:
-- S_var: uses a lookup in our environment and returns it
-- S_neg: -1 * the recursion of q
-- S_sub q1 q2: recursion of q1 minus recursion of q2
-- S_add q1 q2: recursion of q1 plus recursion of q2


-- Originally used double, ucpu1 had problems with it, refer to README.md
transform :: Query -> Env -> Integer 
-- transform :: Query -> Env -> Double 
transform (S_var v) env = case lookup v env of
                            Nothing -> error "Variable not found"
                            Just a -> a
transform (S_neg q) env = -1 * transform q env
transform (S_sub q1 q2) env = transform q1 env - transform q2 env
transform (S_add q1 q2) env = transform q1 env + transform q2 env

-------------------------------------------------------------------------
-- Question 3 -----------------------------------------------------------
-------------------------------------------------------------------------
-- simplifyRules is given a query and returns a query
-- Here is the rule set for the following code:
--
-- Rule Set:
-- A = A                    | -- variable
-- ~(~A) = A                | -- double negation
-- ~A = ~A                  | -- negation
-- (+ ~A ~B) = ~(+ A B)     | -- factorization
-- (- (~A) B) = ~(+ A B)    |
-- (- A (~B)) = (+ A B)     | -- inverting operator (-)
-- (+ (~A) B) = (- B A)     | -- changing order
-- (+ A (~B)) = (- A B)     | -- inverting operator (+)
-- Each line of the rule set matches up to the simplifyRules code

simplifyRules :: Query -> Query
simplifyRules (S_var v) = S_var v
simplifyRules (S_neg (S_neg q)) = simplifyRules q
simplifyRules (S_neg q) = S_neg (simplifyRules q)
simplifyRules (S_add (S_neg q1) (S_neg q2)) = S_neg (S_add (simplifyRules q1) (simplifyRules q2))
simplifyRules (S_sub (S_neg q1) q2) = S_neg (S_add (simplifyRules q1) (simplifyRules q2))
simplifyRules (S_sub q1 (S_neg q2)) = S_add (simplifyRules q1) (simplifyRules q2)
simplifyRules (S_add (S_neg q1) q2) = S_sub (simplifyRules q2) (simplifyRules q1)
simplifyRules (S_add q1 (S_neg q2)) = S_sub (simplifyRules q1) (simplifyRules q2)

------------------------------ Cancellation ----------------------------------------------
-- Cancelling values -> (+ b (a-a)) = b
simplifyRules (S_add q3 (S_sub q1 q2)) = 
    let x = simplifyRules q1
        y = simplifyRules q2
     in if x==y -- if both sub-queries are equal, they cancel, otherwise recurse as normal
           then simplifyRules q3 -- cancel leaves q3
           else S_add (simplifyRules q3) (S_sub (simplifyRules q1) (simplifyRules q2)) 

-- Cancelling values -> (- b (a-a)) = b
simplifyRules (S_sub q3 (S_sub q1 q2)) = 
    let x = simplifyRules q1
        y = simplifyRules q2
     in if x==y -- if both sub-queries are equal, they cancel, otherwise recurse as normal
           then simplifyRules q3 -- cancel leaves only q3
           else S_sub (simplifyRules q3) (S_sub (simplifyRules q1) (simplifyRules q2)) 

-- Cancelling values -> (+ (a-a) b) = b
simplifyRules (S_add (S_sub q1 q2) q3) = 
    let x = simplifyRules q1
        y = simplifyRules q2
     in if x==y -- if both sub-queries are equal, they cancel, otherwise recurse as normal
           then simplifyRules q3 -- cancel leaves only q3
           else S_add (S_sub (simplifyRules q1) (simplifyRules q2)) (simplifyRules q3)

-- Cancelling values -> (- (a-a) b) = -b
simplifyRules (S_sub (S_sub q1 q2) q3) = 
    let x = simplifyRules q1
        y = simplifyRules q2
     in if x==y -- if both sub-queries are equal, they cancel, otherwise recurse as normal
           then S_neg (simplifyRules q3) -- cancel leaves only q3
           else S_sub (S_sub (simplifyRules q1) (simplifyRules q2)) (simplifyRules q3)

-- Other cases for add/sub where no changes are made
simplifyRules (S_sub q1 q2) = S_sub (simplifyRules q1) (simplifyRules q2)
simplifyRules (S_add q1 q2) = S_add (simplifyRules q1) (simplifyRules q2)
-------------------------------------------------------------------------------------------

-- simply is given a query and returns a query
-- this function recurses till no changes are made from simplifyRules
-- q = query, nq = new query
-- If new query = old query then return nq otherwise simplify again.
simplify :: Query -> Query
simplify q =
    let nq = simplifyRules q
     in if nq == q 
           then nq 
           else simplify nq

