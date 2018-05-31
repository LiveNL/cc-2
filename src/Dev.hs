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

--parse :: String -> IO [Stat']
parse :: String -> IO [(Int, [String])]
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  let (freshLabel, t) = sem_Program (happy . alex $ content) 1
  let f = getFlow t
  --let (startLabel, finishLabel) = (freshLabel, freshLabel + 1)
  return (mfp t)

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

transferFunctionExit :: Program' -> Int -> [String]
transferFunctionExit p@(Program' _ s) i | i `elem` (final s) = []
--                                        | otherwise = concatMap (\(l1, l2) -> (transferFunctionEntry p l1)) (reversedFlow s)
                                        | otherwise = concatMap (\(ll1, ll2) -> transferFunctionEntry p ll1) (filter (\(l1, l2) -> l2 == i) (reversedFlow s))

transferFunctionEntry :: Program' -> Int -> [String]
transferFunctionEntry p@(Program' _ s) i = ((transferFunctionExit p i) L.\\ kill block) ++ gen block
    where block = getStat' i (programToStat p)

allVars :: Stat' -> [String]
allVars (Skip' _) = []
allVars (IAssign' _ _ val) = freeVarsI val
allVars (BAssign' _ _ val) = freeVarsB val
allVars _ = []


getLabel (Skip' i) = [i]
getLabel (IAssign' i _ _ ) = [i]
getLabel (BAssign' i _ _ ) = [i]
getLabel (IfThenElse' i _ s1 s2) = [i] ++ getLabel s1 ++ getLabel s2
getLabel (While' i _ s) = [i] ++ (getLabel s)
getLabel (Seq' s1 s2) = (getLabel s1)  ++ (getLabel (s2))

-- mfp
-- needed:
-- list of all statement "programToStat" - complete latice
-- ??? monotone function space? tranferfunctionExit? and tranferfunctionEntry
-- edges "flow of program" - trasition of the program
-- "int "  - extremal label
-- empty list - extremal value
-- map from list of labels to ttransfer functions

-- mfp ::    [Stat'] -- nodes - complete latice
   --    -> [Int] -- final program - extremal labels
     --  -> [] -- extremal value
       -- -> [(Int, Int)] -- flow/edges - transitions
       -- -> Int -- ??? outcome

mfp p@(Program' _ s) =
            let lattice       = allVars s -- moet nog
                transitions    = reversedFlow s
                extremalLabel = final s
                extremalValue = []
                bottemList    = map (\x -> (x,[])) ((getLabel s) L.\\ extremalLabel)
                extremalList  = map (\x -> (x,[])) extremalLabel
                a             = bottemList ++ extremalList
             in  mfpIteration  transitions a s p

getStat l s =  getStat' l (statToStatList s)


getStat' l ((Skip' i): xs) | l == i = Skip' i
                           | otherwise = getStat' l xs
getStat' l ((IAssign' i a b): xs) | l == i = IAssign' i a b
                                  | otherwise = getStat' l xs
getStat' l ((BAssign' i a b): xs) | l == i = BAssign' i a b
                                  | otherwise = getStat' l xs
getStat' l ((IfThenElse' i c s1 s2): xs) | l == i = IfThenElse' i c s1 s2
                                         | otherwise = getStat' l xs
getStat' l ((While' i c s): xs) | l == i = While' i c s
                                | otherwise = getStat' l xs
getStat' l ((Seq' s1 s2): xs)  = getStat' l xs
getStat' _ _ = Skip' 0


mfpIteration :: [(Int, Int)] -> [(Int, [String])] -> Stat' -> Program' -> [(Int, [String])]
mfpIteration [] a s p    = a
mfpIteration w@((l1,l2) : xs) a s p = let a1 = concatMap (\(x1,x2) -> if x1 == l1 then x2 else [])  a  -- allVars (getStat l1 s)
                                          a2 = concatMap (\(x1,x2) -> if x1 == l2 then x2 else [])  a
                                          x1  = (a1 L.\\ kill (getStat l1 s)) ++ gen (getStat l1 s)
                                          superset = getSuperset a2 x1
                                          newA2 = if not superset then (l1, a2 ++ x1) else (l2, a2)
                                          removeA2 = filter (\(x, y) -> l2 /= x) a
                                          newA = a ++ [newA2]
                                      in  mfpIteration xs newA s p



getSuperset :: [String] -> [String] -> Bool
getSuperset _ [] = True
getSuperset x (a : as) | a `elem` x = getSuperset x as
                       | otherwise = False
