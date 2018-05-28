module Dev where

import qualified Data.Map as M
import qualified Data.List as L

import AttributeGrammar
import Lexer
import Main
import Parser

-- To make it all compile for the moment:
type Analysis a = [a]

{-- How To Run (examples)

-- Strongly Live Variables
ghci> run slv "fib"

--}

slv = undefined
cp  = undefined

run :: (Eq a, Show a) => (Program' -> Analysis a) -> String -> IO ()
run = runAnalysis'

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: (Eq a, Show a) => (Program' -> Analysis a) -> String -> IO ()
runAnalysis' analyze programName = do
  p <- parse programName
  putStrLn "OUTPUT:"
  putStrLn (show p)
  putStrLn "G'bye"

-- parse program

parse :: String -> IO [Stat']
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  let (freshLabel, t) = sem_Program (happy . alex $ content) 1
  let f = getFlow t
  --let (startLabel, finishLabel) = (freshLabel, freshLabel + 1)
  return (programToStat t)

getFlow (Program' p s) = flow s




--toStatPrime (Skip) i = Skip' i 
--toStatPrime (IAssign a b) i = IAssign' i a b
--toStatPrime (BAssign a b) i = BAssign' i a b
--toStatPrime (IfThenElse c s1 s2) i = IfThenElse' i c (toStatPrime s1 (i+1)) (toStatPrime s2 (i+1))
--toStatPrime (While c s) i = While' i c (toStatPrime s (i+1))
--toStatPrime (Call  s) i = While' i c (toStatPrime s (i+1))
--toStatPrime (IAssign c s) i = While' i c (toStatPrime s (i+1))
-- toStatPrime (BAssign c s) i = While' i c (toStatPrime s (i+1))
--toStatPrime (Seq s1 s2) i = Seq' (toStatPrime s1 (i+1))  (toStatPrime s2 (i+1))
--toStatPrime _ i = Skip' i
--toStatPrime (Malloc c s) i = While' i c (toStatPrime s (i+1))
--toStatPrime (Free c s) i = While' i c (toStatPrime s (i+1))
--toStatPrime (RefAssign c s) i = While' i c (toStatPrime s (i+1))


-- write in AG
initStat:: Stat' -> Int 
initStat (Skip' i) = i
initStat (IAssign' i _ _) = i
initStat (BAssign' i _ _) = i
initStat (IfThenElse' i _ _ _) = i
initStat (While' i _ _) = i
initStat (Seq' s1 _) = initStat (s1)

final:: Stat' -> [Int] 
final (Skip' i) = [i]
final (IAssign' i _ _ ) = [i]
final (BAssign' i _ _ ) = [i]
final (IfThenElse' i _ s1 s2) = final s1 ++ final s2
final (While' i _ _) = [i]
final (Seq' _ s2) = final (s2)

flow:: Stat' -> [(Int, Int)]
flow (Skip' i) = []
flow (IAssign' i _ _ ) = []
flow (BAssign' i _ _ ) = []
flow (IfThenElse' i _ s1 s2) = flow s1 ++ flow s2 ++ [(i, initStat s1),(i, initStat s2)]
flow (While' i c s) = flow s ++ [(i,initStat s)] ++ map (\x -> (x,i)) (final s)
flow (Seq' s1 s2) = flow s1 ++ flow s2 ++  map (\x -> (x,initStat s2)) (final s1)

reversedFlow :: Stat' -> [(Int, Int)]
reversedFlow s = map (\(x,y)-> (y,x)) (flow s)

-- complete lattice:

-- misschien moeten de statements uit de toegevoegde onderdelen nog wel en alleen label en cnd worden
programToStat :: Program' -> [Stat']
programToStat (Program' _ s) =  statToStatList s

statToStatList (Skip' i)= [Skip' i] 
statToStatList (IAssign' i a b) = [IAssign' i a b]
statToStatList (BAssign' i a b) = [BAssign' i a b]
statToStatList (IfThenElse' i c s1 s2)= [IfThenElse' i c s1 s2] ++ (statToStatList s1 ) ++ (statToStatList s2)
statToStatList (While' i c s) = [While' i c s] ++ (statToStatList s)
statToStatList (Seq' s1 s2) = (statToStatList s1)  ++ (statToStatList s2)
statToStatList _ = [Skip' 0]

-- kill get live variables

gen :: Stat' -> [String]
gen (Skip' _) = []
gen (IAssign' _ name _) = [name]
gen (BAssign' _ _ val) = []
gen _ = []


kill :: Stat' -> [String]
kill (Skip' _) = []
kill (IAssign' _ _ val) = freeVarsI val
kill (BAssign' _ _ val) = freeVarsB val
kill _ = []


freeVarsI :: IExpr -> [String]
freeVarsI (IConst v) = []
freeVarsI (Var n) = [n]
freeVarsI (Plus x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Minus x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Times x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Divide x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Deref x) = freeVarsI x


freeVarsB :: BExpr -> [String]
freeVarsB (BConst v) =[]
freeVarsB (BVar n) = [n]
freeVarsB (LessThan x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (GreaterThan x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (LessEqual x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (GreaterEqual x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (IEqual x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (BEqual x y) = L.nub ((freeVarsB x) ++ (freeVarsB y))
freeVarsB (And x y) = L.nub ((freeVarsB x) ++ (freeVarsB y))
freeVarsB (Or x y) = L.nub ((freeVarsB x) ++ (freeVarsB y))
freeVarsB (Not x) = freeVarsB x

-- tranfer functions live variable 

tranferFunctionExit :: Program' -> Int -> [String]
tranferFunctionExit p@(Program' _ s) i | i `elem` (final s) = []
                                        | otherwise = concat (map (\(l1, l2) -> (transferFunctionEntry p l1)) (reversedFlow s))

transferFunctionEntry :: Program' -> Int -> [String]
transferFunctionEntry p@(Program' _ s) i = ((tranferFunctionExit p i) L.\\ (concat (map kill (programToStat p)))) ++  (concat (map gen (programToStat p)))

