import Data.Map (Map)
import qualified Data.Map as Map

------------------------------- Q1 ------------------------------------------------------------------------

-- Importing an efficient implementation of maps from keys to values (dictionaries).
import Data.Maybe
-- To use fromJust function

product_ :: [Int] -> Int
-- Product takes a list of integers and returns an integer
product_ xs = foldr (*) 1 xs
-- e.g. a:b:c:..:z:[] ->   (a*(b*(c*..+(z*1))))          1
max_ :: [Int] -> Int
-- a function from a list of integers to an integer
max_ [] = 0
-- base case
max_ xs = foldr max_2 minBound xs
             where max_2 a b =
                     if a<b then b
                     else a
-- e.g. a:b:c:..:z:[] ->   (a `max_2` (b `max_2` (c`max_2` ..+(z`max_2`minBound)))) , minBound predefined as the smallest Int/Integer

min_ :: [Int] -> Int
min_ [] = 0
min_ xs = foldr (min_2) maxBound xs
            where min_2 a b=
                    if (a<b) then a
                    else b
-- e.g. a:b:c:..:z:[] ->   (a `min_2` (b `min_2` (c`min_2` ..+(z`min_2`minBound)))) , maxBound predefined as the biggest Int/Integer

card :: [Int] -> Map Int Int
card []     = Map.empty
card (c:cs) = countChar c (card cs)
  where countChar c oldMap = Map.insertWith (+) c 1 oldMap
-- go through each integer and increment corresponding value


cardf :: [Int] -> Map Int Int
cardf [] = Map.empty
cardf xs = foldr (\c oldMap->  Map.insertWith (+) c 1 oldMap) Map.empty xs
--  makes use of anonymous function to map an element and a map to a new map with an incremented value
mode :: [Int] -> [Int]
mode xs  = Map.keys (Map.filter (== max_(Map.elems(cardf xs)))  (cardf xs))
--use cardf to get map, get keys whose value is the highest, e.g.fromList [(5,4),(1,2),(2,4)] -> [5,2]

---------------------______________________________________________________________-------------------------



---------------------------------------------------Q2 and Q3----------------------------------------------


type Var = String
-- variables
type PC = Int
-- program counter
type Globals = Map Var Const
-- a mapping between global vars and their values
acc :: Var
-- accumulator
acc = "_acc"
-- definining accumlator
type BytecodeProgram = [Inst]
-- a bytecode program is a list of instuctions

class Executable a where
  exec :: a -> (Globals, PC) -> Maybe (Globals, PC)

-- type class executble


simpleExec :: BytecodeProgram -> Globals -> Maybe Globals
simpleExec [] curGlob = Just curGlob
simpleExec s@(x:xs) curGlob = simpleExec xs (first (exec x (curGlob,0)))
  where
    first (Just (a,b)) = a
    first (Nothing) = curGlob
-- simplExec a funciton that executes the instructions recustively

-- type Const = Int -- Modified in Q3
data Const =  INTEGER Int
  | PROGRAM BytecodeProgram
  deriving(Eq, Show)
-- Const is an integer or a program
{- --Modified in Q3
data Inst = 
  LOAD_CONST Const
  | LOAD_GLOBAL Var 
  | STORE_GLOBAL Var
  | CALL_FUNCTION
  | JUMP PC
  | JUMP_IF_FALSE PC
  | ADD Var
  | SUB Var
  deriving (Eq, Show)
-}
data Inst =
  LOAD_CONST Const
  | LOAD_GLOBAL Var 
  | STORE_GLOBAL Var
  | CALL_FUNCTION
  | JUMP PC
  | JUMP_IF_FALSE PC
  | EXCNEQL Var Var Const -- optimisation for q4 where 1st 2nd var to compair and 3rd contians program to execute
  | ADD Var
  | SUB Var
  deriving (Eq, Show)
-- List of possible instructions for the Bytecodeprogram



{- --Modified in Q3
instance Executable Inst where
  -- load value in accumulator 
  exec (LOAD_CONST x) (curGlob,curPc) = Just  (newGlob,curPc+1)
    where newGlob = Map.insert acc x curGlob
  -- get value from acc and store it int var
  exec(STORE_GLOBAL var) (curGlob,curPc) =
    case (Map.lookup acc curGlob) of
      Nothing -> Nothing
      otherwise -> Just ((Map.insert var (fromJust (Map.lookup acc curGlob)) curGlob), curPc+1)
  exec(LOAD_GLOBAL var) (curGlob, curPc) =
    case (Map.lookup var curGlob) of
      Nothing -> Nothing
      otherwise -> Just ((Map.insert acc  (fromJust (Map.lookup var curGlob)) curGlob),curPc+1)
  exec (JUMP_IF_FALSE pc) (curGlob,curPc) =
    case (Map.lookup acc curGlob) of
      Just 0 -> Just (curGlob, pc)
      otherwise -> Nothing
  exec (ADD var) (curGlob,curPc) =
    case (Map.lookup acc curGlob) of
      Nothing -> Nothing
      otherwise ->
        case (Map.lookup var curGlob) of
          Nothing -> Nothing
          otherwise -> Just (Map.insert acc (fromJust (Map.lookup acc curGlob) + fromJust (Map.lookup var curGlob)) curGlob ,curPc+1)
  exec (SUB var) (curGlob,curPc) =
    case (Map.lookup acc curGlob) of
      Nothing -> Nothing
      otherwise ->
        case (Map.lookup var curGlob) of
          Nothing -> Nothing
          otherwise -> Just (Map.insert acc (fromJust (Map.lookup acc curGlob) -  fromJust (Map.lookup var curGlob)) curGlob ,curPc+1)
-}

-- Inst an instance of typeclass executable
instance Executable Inst where 

  exec (LOAD_CONST x) (curGlob,curPc) = Just  (newGlob,curPc+1)
    where newGlob = Map.insert acc x curGlob
  -- load value x in acc
  
  exec(STORE_GLOBAL var) (curGlob,curPc) =
    case (Map.lookup acc curGlob) of
         Nothing -> Nothing
         otherwise -> Just ((Map.insert var (fromJust (Map.lookup acc curGlob)) curGlob), curPc+1)
  -- store the value of acc in var

  exec(LOAD_GLOBAL var) (curGlob, curPc) =
    case (Map.lookup var curGlob) of
      Nothing -> Nothing
      otherwise -> Just ((Map.insert acc (fromJust (Map.lookup var curGlob)) curGlob),curPc+1)
  -- load the value of var in acc
     
  exec (JUMP_IF_FALSE pc) (curGlob,curPc) =
    case (Map.lookup acc curGlob)  of
      Just (INTEGER 0) -> Just (curGlob,pc)
      othewise -> Nothing
 -- if the value in the accumulator is 0 then change the pc
  
  exec (ADD var) (curGlob,curPc) =
    case(Map.lookup acc curGlob) of
      Just (INTEGER a) ->
         case(Map.lookup var curGlob) of
           Just (INTEGER b) -> Just ((Map.insert acc (INTEGER (a+b)) curGlob) ,curPc+1)
           otherwise -> Nothing
      otherwise -> Nothing
  -- extra function added for fun
  
  exec (SUB var) (curGlob,curPc) =
    case(Map.lookup acc curGlob) of
      Just (INTEGER a) ->
         case(Map.lookup var curGlob) of
           Just (INTEGER b) -> Just ((Map.insert acc (INTEGER (a-b)) curGlob) ,curPc+1)
           otherwise -> Nothing
      otherwise -> Nothing    
-- sub the value in the accumulator by the value in var, check if the values are integers or programs and handle accordingly

  exec (CALL_FUNCTION) (curGlob,curPc) =
    case (Map.lookup acc curGlob) of
      Just (PROGRAM p) ->
        case (simpleExec p curGlob) of
          Just a -> Just (a,curPc+1)  
      otherwise -> Nothing
  -- exec call function where if a program is loaded in the accumulator then we execute that program otherwise -> Nothing

  
  -- The following was added in conjunciton to Q4 as an optimisation.------------------------------------
  exec(EXCNEQL a b c) (curGlob,curPc) =
    if( (Map.lookup a curGlob)/= (Map.lookup b curGlob)) && ( (Map.lookup a curGlob) /= Nothing)&& ( (Map.lookup b curGlob) /= Nothing) 
    then (case c of
            INTEGER y -> Nothing
            PROGRAM x -> exec(CALL_FUNCTION) ((Map.insert acc (PROGRAM x) curGlob),curPc))
    else Nothing
-----------------------------------------------------------------------------------------------
    
  

-----------------------------------------------------Q4---------------------------------------------------
infixl 5 :=
type HighLevelProgram = [Stmt]
data Stmt = Def Var HighLevelProgram 
            | Var := Expr
            | Return Expr
            | If Expr [Stmt]
            | While Expr [Stmt]            
data Expr = V Var
            | C Int
            | Call Var Expr
            | Minus Expr Expr
            deriving (Eq,Show)
compile :: HighLevelProgram -> BytecodeProgram
compile [] = []
compile (x:xs) = case x of
                    Def s p -> (LOAD_CONST (PROGRAM (compile p))):(STORE_GLOBAL s):(compile xs)

                    s := (C q) -> (LOAD_CONST (INTEGER q)):(STORE_GLOBAL s):(compile xs)
                    s := (V q) -> (LOAD_GLOBAL(q)):(STORE_GLOBAL s):(compile xs)
                
                    s := Minus (C q) (C l) ->  (LOAD_CONST (INTEGER (q-l))):(STORE_GLOBAL s):(compile xs)
                    s := Minus (V q) (V l) -> (LOAD_GLOBAL(q)):(SUB l):(STORE_GLOBAL s):(compile xs) 
                    s := Minus (V q) (C l) -> (LOAD_CONST (INTEGER l)):(STORE_GLOBAL "TMP")
                      :(LOAD_GLOBAL q):(SUB "TMP"):(STORE_GLOBAL s):(compile xs)
                    s := Minus (C l) (V q) -> (LOAD_CONST (INTEGER l)):(SUB q):(STORE_GLOBAL s):(compile xs)
                    
                    s := Call f a -> LOAD_CONST(PROGRAM (compile ([Return (Call f a)]))):(STORE_GLOBAL f):(compile xs)
                    
                    Return (V s) -> (LOAD_GLOBAL s):(compile xs)
                    Return (C a) -> (LOAD_CONST (INTEGER a)):(compile xs)
                    Return (Call f expr) -> (case expr of
                                              (C a) ->( (LOAD_GLOBAL f):[(CALL_FUNCTION)] )
                                              (V v) -> ((LOAD_GLOBAL f):(CALL_FUNCTION):(LOAD_GLOBAL v) :[(CALL_FUNCTION)])
                                              (Call f2 expr2) -> (compile( [Return (Call f2 expr2)]))
                                              (Minus a b) -> []

                                            )++(compile xs)
                    Return (Minus a b) -> (compile [acc := Minus a b])++(compile xs)
                    If (C x) p ->  case x of
                      0 -> compile xs
                      otherwise ->LOAD_CONST(PROGRAM (compile p)):CALL_FUNCTION:(compile xs)
                   

                    If (V v) p -> (LOAD_CONST(INTEGER 0)):(STORE_GLOBAL "TMP"):(EXCNEQL "TMP" v (PROGRAM (compile p))):(compile xs)

                     {-
                     Remember that loops dont exists in the functional world
                     -> Make use of recursion
                    -}
                    While (C 0) p -> (compile xs)
                    -- C 0 -> dont execute
                    While (C x) p ->  LOAD_CONST(PROGRAM (compile p)):CALL_FUNCTION:(compile ([(While (C (x)) p)]))
                    -- infinite loop
                    -- can result in an infinite loop
                    While (V v) p -> (LOAD_CONST(INTEGER 0)):(STORE_GLOBAL "TMP"):(EXCNEQL "TMP" v (PROGRAM (compile (p++ [While(V v) p])))):(compile xs)
                    -- can result in an inifinite loop
                                       
                    While (Minus (a) (b)) p -> ((LOAD_CONST(INTEGER 0)):[(STORE_GLOBAL "TMP")])++ (compile(["TMP2" := Minus (a) (b)])) ++ ( (EXCNEQL "TMP" "TMP2" (PROGRAM (compile (p++ [While(V "TMP2") p]))))):(compile xs)


                   
                    otherwise -> (compile xs)

-------------------________________________________________________________________________---------------




------------------------- Test cases and outputs --------------------------------------------------------------
-- Test cases and outputs for Q1

-- λ> product_ [1,4,2]
-- 8

-- λ> max_[3,-1,8,2]
-- 8

-- λ> min_[3,-1,8,2]
-- -1

-- λ> card [1,2,3,3,1,1,2,2,3,5,1]
-- fromList [(1,4),(2,3),(3,3),(5,1)]

-- λ> cardf [1,2,3,3,1,1,2,2,3,5,1]
-- fromList [(1,4),(2,3),(3,3),(5,1)]

-- λ> mode [1,2,3,2,1,-1,3,3,-1,1]
-- [1,3]


-- Test cases and outputs for Q2 & Q3



program1 = [LOAD_CONST (INTEGER 3)]

-- λ> simpleExec(program1) Map.empty
-- Just (fromList [("_acc",INTEGER 3)])

program2 = [LOAD_CONST (PROGRAM program1), STORE_GLOBAL "f", LOAD_GLOBAL "f", CALL_FUNCTION]

-- λ> simpleExec(program2) Map.empty
-- Just (fromList [("_acc",INTEGER 3),("f",PROGRAM [LOAD_CONST (INTEGER 3)])])

program3 = [LOAD_CONST (INTEGER 19), STORE_GLOBAL "f", LOAD_CONST (INTEGER 25), ADD "f"]

-- λ> simpleExec(program3) Map.empty
--Just (fromList [("_acc",INTEGER 44),("f",INTEGER 19)])

program4 = [LOAD_CONST (PROGRAM program1), STORE_GLOBAL "f", LOAD_GLOBAL "f", CALL_FUNCTION]

-- λ> simpleExec(program4) Map.empty
-- Just (fromList [("_acc",INTEGER 3),("f",PROGRAM [LOAD_CONST (INTEGER 3)])])



-- Test cases and outputs for Q4
h_program0 = ["my_var" := (C 2), "my_var" := (C 5) `Minus` (V "my_var")]
-- λ> simpleExec(compile h_program0) Map.empty
-- Just (fromList [("_acc",INTEGER 3),("my_var",INTEGER 3)])

h_program1 = ["my_var" := (C 2), "my_var" := (C 5) `Minus` (V "my_var"), Return (V "my_var")]
-- λ> simpleExec(compile h_program1) Map.empty
-- Just (fromList [("_acc",INTEGER 3),("my_var",INTEGER 3)])

h_program2 = [Def "f" h_program1, Return (Call "f" (C 0))]
--λ> simpleExec(compile h_program2) Map.empty
--Just (fromList [("_acc",INTEGER 3),("f",PROGRAM [LOAD_CONST (INTEGER 2),STORE_GLOBAL "my_var",LOAD_CONST (INTEGER 5),SUB "my_var",STORE_GLOBAL "my_var",LOAD_GLOBAL "my_var"]),("my_var",INTEGER 3)])

h_program3 = ["functionCall":=(Call "f" (C 0) )]
--λ> simpleExec(compile h_program3) Map.empty
--Just (fromList [("_acc",PROGRAM [LOAD_GLOBAL "f",CALL_FUNCTION]),("f",PROGRAM [LOAD_GLOBAL "f",CALL_FUNCTION])])

h_program4 = ["my_var" := (C 10),(While (C 2) ["my_var" := (V "my_var") `Minus` (C 2)] )]
-- keeps on compiling forever

h_program5 = ["my_var" := (C 5), "my_var" := (C 4) `Minus` (V "my_var"), If (V "my_var") ["nice" := (C 5)]  ]
-- λ> simpleExec(compile h_program5) Map.empty
-- Just (fromList [("TMP",INTEGER 0),("_acc",INTEGER 5),("my_var",INTEGER (-1)),("nice",INTEGER 5)])

h_program6 = ["my_var" := (C 5), "my_var" := (C 5) `Minus` (V "my_var"), If (V "my_var") ["nice" := (C 5)]  ]
-- λ> simpleExec(compile h_program6) Map.empty
-- Just (fromList [("TMP",INTEGER 0),("_acc",INTEGER 0),("my_var",INTEGER 0)])

h_program7 = ["my_var" := (C 5),"nice" := (C 12), While (V "my_var") ["my_var" := (V "my_var") `Minus` (C 1), "nice" := (V "nice") `Minus` (C 1) ] , "cool" := (C 7) ]
-- λ> simpleExec(compile h_program7) Map.empty
-- Just (fromList [("TMP",INTEGER 0),("_acc",INTEGER 7),("cool",INTEGER 7),("my_var",INTEGER 0),("nice",INTEGER 7)])

h_program8 = ["my_var" := (C 12),"nice" := (C 12), While ((V "my_var") `Minus` (V "nice")) ["cool" := (C 7) ]]
-- λ> simpleExec(compile h_program8) Map.empty
-- Just (fromList [("TMP",INTEGER 0),("TMP2",INTEGER 0),("_acc",INTEGER 0),("my_var",INTEGER 12),("nice",INTEGER 12)])

h_program9 = ["my_var" := (C 5),"nice" := (C 12), While (V "my_var") ["my_var" := (V "my_var") `Minus` (C 1), "nice" := (V "nice") `Minus` (C 1) ] , "cool" := (C 7) ]
-- λ> simpleExec(compile h_program9) Map.empty
-- Just (fromList [("TMP",INTEGER 0),("_acc",INTEGER 7),("cool",INTEGER 7),("my_var",INTEGER 0),("nice",INTEGER 7)])


--------------------------______________________________________________-----------------------------------
