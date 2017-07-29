-----------------------------------------------------------
--Testing Script for Assignment----------------------------
-----------------------------------------------------------
-- Importing Assignment.hs and Haskell's test unit
import Assignment1
import Test.HUnit

----------------------------------------------------------
--Setup for Assignment----------------------------
-----------------------------------------------------------
-- define some variables
a = "d"
b = "e"
c = "f"

-- Default Environment(s)
myEnv = [(a, 4), (b,10), (c,-1)]

-- My Test Queries
myQ = S_neg (S_neg (S_var a))
myQ1 = S_neg (S_sub (S_var a) (S_var b)) 
myQ2 = S_neg (S_add (S_neg (S_var a)) (S_neg (S_var b)))
myQ3 = S_neg (S_sub (S_neg (S_var a)) (S_var b))
myQ4 = S_add (S_neg (S_add (S_neg (S_var a)) (S_neg (S_var b)))) (S_var c)
myQ5 = S_add (S_neg (S_var a)) (S_var a)
myQ6 = S_sub (S_var a) (S_var a)
myQ7 = S_sub (S_var a) (S_var b)
myQ8 = S_neg (S_sub (S_var a) (S_sub (S_var a) (S_var a))) 
myQ9 = S_neg (S_sub (S_sub (S_var a) (S_var a)) (S_var a)) 
myQ10 = S_add (S_neg (S_sub (S_neg (S_var a)) (S_neg (S_var a)))) (S_var c)
myQ11 = S_neg (S_neg (S_sub (S_var b) (S_var a)))
myQ12 = S_sub (S_var a) (S_sub (S_var a) (S_var a))
myQ13 = S_sub (S_sub (S_var a) (S_var a)) (S_sub (S_var a) (S_var a))
myQ14 = S_sub (S_sub (S_var a) (S_var a)) (S_var b)
myQ15 = S_add (S_var b) (S_add (S_var b) (S_var c))
myQ16 = S_add (S_neg (S_var b)) (S_add (S_neg(S_var b)) (S_var c))



-- List of queries
myQueries = [myQ,myQ1,myQ2,myQ3,myQ4,myQ5,myQ6,myQ7,myQ8,myQ9,myQ10,myQ11,myQ12,myQ13,myQ14,myQ15,myQ16]
numOfTests = [0..16]

-----------------------------------------------------------
--Testing findVars-----------------------------------------
-----------------------------------------------------------

-- Expected Results for findVars
findVarsExpected = 
    [
    [a],        --myQ
    [a,b],      --myQ1
    [a,b],      --myQ2
    [a,b],      --myQ3
    [a,b,c],    --myQ4
    [a],        --myQ5
    [a],        --myQ6
    [a,b],      --myQ7
    [a],        --myQ8
    [a],        --myQ9
    [a,c],      --myQ10
    [b,a],      --myQ11
    [a],        --myQ12
    [a],        --myQ13
    [a,b],      --myQ14
    [b,c],      --myQ15
    [b,c]
    ]

-- Test list uses this format for each test
-- Following the Haskell HUnit's 1.0 Users Guide: 3.4 Additional Features
-- ~: -> attaches a label that is testable
-- @?= or @=? makes an assertEquals between the expected result and the query
-- parsing through a list of couples from 3 lists 
findVarsTests = 
    TestList [
    "test" ++ show n ~: "(findVars : " ++ show q ++ ")" ~: (findVars q) @?= e | (n,e,q) <- zip3 numOfTests findVarsExpected myQueries
             ]

-----------------------------------------------------------
--Testing transform----------------------------------------
-----------------------------------------------------------

-- Expected Results for transform
transformExpected = 
    [
    4,          --myQ
    6,          --myQ1
    14,         --myQ2
    14,         --myQ3
    13,         --myQ4
    0,          --myQ5
    0,          --myQ6
    -6,         --myQ7
    -4,         --myQ8
    4,          --myQ9
    -1,         --myQ10
    6,          --myQ11
    4,          --myQ12
    0,          --myQ13
    -10,        --myQ14
    19,         --myQ15
    -21         --myQ16 
    ]

-- transform format for each test
transformTests = 
    TestList [
    "test" ++ show n ~: "(transform : " ++ show q ++ ")" ~: (transform q myEnv) @?= e | (n,e,q) <- zip3 numOfTests transformExpected myQueries
             ]

-----------------------------------------------------------
--Testing simplify-----------------------------------------
-----------------------------------------------------------

-- Expected Results for findVars
simplifyExpected = 
    [
    S_var a,                                       --myQ
    S_neg (S_sub (S_var a) (S_var b)),             --myQ1
    S_add (S_var a)  (S_var b),                    --myQ2
    S_add (S_var a)  (S_var b),                    --myQ3
    S_add (S_add (S_var a) (S_var b)) (S_var c),   --myQ4
    S_sub (S_var a) (S_var a),                     --myQ5
    S_sub (S_var a) (S_var a),                     --myQ6
    S_sub (S_var a)  (S_var b),                    --myQ7
    S_neg (S_var a),                               --myQ8
    S_var a,                                       --myQ9
    S_var c,                                       --myQ10
    S_sub (S_var b) (S_var a),                     --myQ11
    S_var a,                                       --myQ12
    S_sub (S_var a) (S_var a),                     --myQ13
    S_neg (S_var b),                               --myQ14
    S_add (S_var b) (S_add (S_var b) (S_var c)),   --myQ15
    S_sub (S_sub (S_var c) (S_var b)) (S_var b)
    ]

-- simplify format for each test
simplifyTests = 
    TestList [
    "test" ++ show n ~: "(simplify : " ++ show q ++ ")" ~: (simplify q) @?= e | (n,e,q) <- zip3 numOfTests simplifyExpected myQueries
             ]


-----------------------------------------------------------
--Run Tests -----------------------------------------------
-----------------------------------------------------------

--Run Unit tests
runTests::IO Counts
runTests = do
    print "Test Results : Question 1 - findVars"
    runTestTT findVarsTests
    print "Test Results : Question 2 - transform"
    runTestTT transformTests
    print "Test Results : Question 3 - simplify"
    runTestTT simplifyTests

