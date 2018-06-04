module Dev where

import qualified Data.Map as M
import qualified Data.List as L

import AttributeGrammar
import Lexer
import Main
import Parser
import Debug.Trace

type Analysis a = [a]

-- run some analysis by passing an analysis function and a 'show' function to display the result
runAnalysis' :: String -> IO ()
runAnalysis' programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  let (label, p) = sem_Program (happy . alex $ content) 1
  putStrLn "OUTPUT:"
  putStrLn "\nSLV:"
  putStrLn (show (slv p))
  putStrLn "\nCP:"
  putStrLn (show (cp p))
  putStrLn "\nG'bye"

----------------------------
-- BASE FUNCTIONS
----------------------------
getAgInit p          = let (finalAg, flowAg, initAg) = sem_Program' p in initAg
getAgFinal p         = let (finalAg, flowAg, initAg) = sem_Program' p in finalAg
getAgFlow p          = let (finalAg, flowAg, initAg) = sem_Program' p in flowAg
getAgInitWithStat s  = let (finalAg, flowAg, initAg) = sem_Stat' s in initAg
getAgFinalWithStat s = let (finalAg, flowAg, initAg) = sem_Stat' s in finalAg
getAgFlowWithStat s _ = let (finalAg, flowAg, initAg) = sem_Stat' s in flowAg

-- write in AG
--initStat :: Stat' -> Int
--initStat (Skip' i)             = i
--initStat (IAssign' i _ _)      = i
--initStat (BAssign' i _ _)      = i
--initStat (IfThenElse' i _ _ _) = i
--initStat (While' i _ _)        = i
--initStat (Seq' s1 _)           = initStat s1
--initStat (Call' i _ _ _ _)     = i
--
--final :: Stat' -> [Int]
--final (Skip' i)               = [i]
--final (IAssign' i _ _ )       = [i]
--final (BAssign' i _ _ )       = [i]
--final (IfThenElse' i _ s1 s2) = final s1 ++ final s2
--final (While' i _ _)          = [i]
--final (Seq' _ s2)             = final s2
--final (Call' _ e _ _ _)       = [e]

initStat s = getAgInitWithStat s
final s = getAgFinalWithStat s

flow :: Stat' -> Procs' -> [(Int, Int)]
flow (Skip' i) _                            = []
flow (IAssign' i _ _ ) _                    = []
flow (BAssign' i _ _ ) _                    = []
flow (IfThenElse' i _ s1 s2) p              = flow s1 p ++ flow s2 p ++ [(i, initStat s1),(i, initStat s2)]
flow (While' i c s) p                       = flow s p ++ [(i,initStat s)] ++ map (\x -> (x,i)) (final s)
flow (Seq' s1 s2) p                         = flow s1 p ++ flow s2 p ++ map (\x -> (x,initStat s2)) (final s1)
flow (Call' i e name params out) pr@(p: ps) = [(i,n),(x,e)]
  where n = fst (findProcLabels name p ps)
        x = snd (findProcLabels name p ps)

findProcLabels name (Proc' e r procName _ _ _) ps = if name == procName
                                                    then (e, r)
                                                    else findProcLabels name (fstP ps) (sndP ps)

fstP (p:ps) = p
sndP (p:ps) = ps

reversedFlow :: Stat' -> Procs' -> [(Int, Int)]
reversedFlow s p = map (\(x,y) -> (y,x)) (flow s p)

programToStat :: Program' -> [Stat']
programToStat (Program' _ s) = statToStatList s

statToStatList (Skip' i)               = [Skip' i]
statToStatList (IAssign' i a b)        = [IAssign' i a b]
statToStatList (BAssign' i a b)        = [BAssign' i a b]
statToStatList (IfThenElse' i c s1 s2) = [IfThenElse' i c s1 s2] ++ (statToStatList s1) ++ (statToStatList s2)
statToStatList (While' i c s)          = [While' i c s] ++ (statToStatList s)
statToStatList (Seq' s1 s2)            = (statToStatList s1)  ++ (statToStatList s2)
statToStatList _                       = [Skip' 0]

getLabel (Skip' i)               = i
getLabel (IAssign' i _ _ )       = i
getLabel (BAssign' i _ _ )       = i
getLabel (IfThenElse' i _ s1 s2) = i
getLabel (While' i _ s)          = i
getLabel (Seq' s1 _)             = getLabel s1
getLabel (Call' i _ _ _ _)       = i

getLabels (Skip' i)               = [i]
getLabels (IAssign' i _ _ )       = [i]
getLabels (BAssign' i _ _ )       = [i]
getLabels (IfThenElse' i _ s1 s2) = [i] ++ getLabels s1 ++ getLabels s2
getLabels (While' i _ s)          = [i] ++ (getLabels s)
getLabels (Seq' s1 s2)            = (getLabels s1) ++ (getLabels s2)
getLabels (Call' i _ _ _ _)       = [i]

getStat l s =  getStat' l (statToStatList s)

getStat' l ((Skip' i): xs)               | l == i = Skip' i
                                         | otherwise = getStat' l xs
getStat' l ((IAssign' i a b): xs)        | l == i = IAssign' i a b
                                         | otherwise = getStat' l xs
getStat' l ((BAssign' i a b): xs)        | l == i = BAssign' i a b
                                         | otherwise = getStat' l xs
getStat' l ((IfThenElse' i c s1 s2): xs) | l == i = IfThenElse' i c s1 s2
                                         | otherwise = getStat' l xs
getStat' l ((While' i c s): xs)          | l == i = While' i c s
                                         | otherwise = getStat' l xs
getStat' l ((Seq' s1 s2): xs) = getStat' l xs
getStat' _ _ = Skip' 0

getVarStatements (Skip' i) _               = []
getVarStatements (IAssign' i s _ ) _       = [s]
getVarStatements (BAssign' i s _ ) _       = [s]
getVarStatements (IfThenElse' i _ s1 s2) p = getVarStatements s1 p ++ getVarStatements s2 p
getVarStatements (While' i _ s) p          = getVarStatements s p
getVarStatements (Seq' s1 s2) p            = getVarStatements s1 p ++ getVarStatements s2 p
getVarStatements (Call' _ _ _ _ _) p       = []
-- getVarStatements (Call' _ _ name _ _ ) pr@(p:ps) = getVarStatements (findProc name p ps) pr

--findProc name p@(Proc' e r procName _ _ s) ps = if name == procName
--                                                then s
--                                                else findProc name (fstP ps) (sndP ps)


----------------------------
-- CP main functions
----------------------------

data Result = Top | Bottum | Num Int deriving (Show, Eq)

cp (Program' p s) =
  let transitions    = flow s p
      extremalLabels = [getAgInitWithStat s]
      extremalValue  = map (\x -> (x, Top)) (getVarStatements s p)
      in mfp (statToStatList s) transitions extremalLabels extremalValue cpTransferFunction [] True True s

cpTransferFunction :: [(String, Result)] -> Int -> Stat' -> [(String, Result)]
cpTransferFunction a1 label stmt =  cpTransferFunction' a1 stmt

cpTransferFunction' :: [(String, Result)] -> Stat' -> [(String, Result)]
cpTransferFunction' s (Skip' _)          = []
cpTransferFunction' s (IAssign' _ x a)  | null s =  []
                                        | otherwise = (removeItem x s) ++ [(x, (acp a s))]
cpTransferFunction' s (BAssign' _ _ _)   = s
cpTransferFunction' s (IfThenElse' i _ s1 s2) = cpTransferFunction' s s1 ++ cpTransferFunction' s s2
cpTransferFunction' s (While' i _ s1)         = cpTransferFunction' s s1
cpTransferFunction' s (Seq' s1 s2)            = cpTransferFunction' s s1 ++ cpTransferFunction' s s2
cpTransferFunction' s (Call' _ _ _ _ _)        = []

removeItem _ []              = []
removeItem x (y@(x1,x2):ys) | x == x1   = removeItem x ys
                            | otherwise = y : removeItem x ys

acp ::  IExpr -> [(String, Result)] -> Result
acp (IConst v) s = Num v
acp (Var n) s = snd (head ((filter (\(x1,x2) -> x1 == n)) s))

acp (Plus a1 a2) s   | topOrNum (acp a1 s) (acp a2 s) = Num ((toVal (acp a1 s)) + (toVal (acp a2 s)))
                     | otherwise = Top
acp (Minus a1 a2) s  | topOrNum (acp a1 s) (acp a2 s) = Num ( (toVal (acp a1 s)) - (toVal (acp a2 s)))
                     | otherwise = Top
acp (Times a1 a2) s  | topOrNum (acp a1 s) (acp a2 s) = Num ( (toVal (acp a1 s)) * (toVal (acp a2 s)))
                     | otherwise = Top
acp (Divide a1 a2) s | topOrNum (acp a1 s) (acp a2 s) = Num (div (toVal (acp a1 s)) (toVal (acp a2 s)))
                     | otherwise = Top

topOrNum (Num _) (Num _) = True
topOrNum _ _             = False

toVal (Num x) = x


----------------------------
-- SLV
----------------------------
gen :: Stat' -> [String]
gen (Skip' _)           = []
gen (IAssign' _ name _) = [name]
gen (BAssign' _ _ val)  = []
gen _                   = []

kill :: Stat' -> [String]
kill (Skip' _)          = []
kill (IAssign' _ _ val) = freeVarsI val
kill (BAssign' _ _ val) = freeVarsB val
kill _                  = []

freeVarsI :: IExpr -> [String]
freeVarsI (IConst v)   = []
freeVarsI (Var n)      = [n]
freeVarsI (Plus x y)   = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Minus x y)  = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Times x y)  = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Divide x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsI (Deref x)    = freeVarsI x

freeVarsB :: BExpr -> [String]
freeVarsB (BConst v)         = []
freeVarsB (BVar n)           = [n]
freeVarsB (LessThan x y)     = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (GreaterThan x y)  = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (LessEqual x y)    = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (GreaterEqual x y) = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (IEqual x y)       = L.nub ((freeVarsI x) ++ (freeVarsI y))
freeVarsB (BEqual x y)       = L.nub ((freeVarsB x) ++ (freeVarsB y))
freeVarsB (And x y)          = L.nub ((freeVarsB x) ++ (freeVarsB y))
freeVarsB (Or x y)           = L.nub ((freeVarsB x) ++ (freeVarsB y))
freeVarsB (Not x)            = freeVarsB x

slv (Program' p s) =
  let transitions   = reversedFlow s p
      extremalLabels = final s
      extremalValue = []
      bottomList    = map (\x -> (x, [])) ((getLabels s) L.\\ extremalLabels)
      extremalList  = map (\x -> (x, [])) extremalLabels
      a             = bottomList ++ extremalList
   in mfp (statToStatList s) transitions extremalLabels extremalValue slvTransferFunction [] True True s

slvTransferFunction :: [String] -> Int -> Stat' -> [String]
slvTransferFunction a1 label stm = (a1 L.\\ kill (getStat label stm)) ++ gen (getStat label stm)


----------------------------
-- MFP CODE
----------------------------
mfp :: Eq a => [Stat'] -> [(Int,Int)] -> [Int] -> [a] -> ([a] -> Int -> Stat' -> [a]) -> [a] -> Bool -> Bool -> Stat'
    -> [(Int, [a])]
mfp stms transitions extremalLabels extremalValue transferFunctions bottom setType ion s =
  let a = mfpInit s extremalLabels extremalValue bottom
      w = transitions
   in mfpIteration a w transferFunctions s setType ion

mfpInit :: Stat' -> [Int] -> [a] -> [a] ->  [(Int, [a])]
mfpInit statements extremalLabels extremalValue bottom = setA statements
  where setA stmt = if (getLabel stmt) `elem` extremalLabels
                    then map (\x -> (x, extremalValue)) extremalLabels
                    else map (\x -> (x, [])) ((getLabels stmt) L.\\ extremalLabels)

mfpIteration :: Eq a => [(Int , [a])] -> [(Int,Int)] -> ([a] -> Int -> Stat' -> [a]) -> Stat' -> Bool -> Bool -> [(Int , [a])]
mfpIteration a [] t s set ion  = a
mfpIteration a w@((l1, l2) : ws) transferFunctions stms setType ion =
  let a1 = concatMap (\(x1,x2) -> if x1 == l1 then x2 else []) a
      a2 = concatMap (\(x1,x2) -> if x1 == l2 then x2 else []) a
      fla1 = transferFunctions a1 l1 stms
   in if setType == True
      then mfpIteration (updateA (checkSuperSet (isSuperSet fla1 a2) a2 fla1)) ws transferFunctions stms setType ion
      else mfpIteration (updateA (checkSuperSet (isSuperSet a2 fla1) a2 fla1)) ws transferFunctions stms setType ion
        where checkSuperSet bool a2 fla1 | bool = if ion == True
                                                  then (l1, a2 ++ fla1)
                                                  else (l2, a2 L.\\ fla1)
                                         | otherwise = (l2, a2)
              updateA newA = (filter (\(x, y) -> l2 /= x) a) ++ [newA]

isSuperSet :: Eq a => [a] -> [a] -> Bool
isSuperSet _ [] = True
isSuperSet x (a:as) | a `elem` x = isSuperSet x as
                    | otherwise = False


----------------------------
-- BASIC LIVE VARIABLES IMPLEMENTATION
----------------------------
transferFunctionExit :: Program' -> Int -> [String]
transferFunctionExit p@(Program' procs s) i | i `elem` (final s) = []
  | otherwise = concatMap (\(ll1, ll2) -> transferFunctionEntry p ll1) (filter (\(l1, l2) -> l2 == i) (reversedFlow s procs))

transferFunctionEntry :: Program' -> Int -> [String]
transferFunctionEntry p@(Program' _ s) i = ((transferFunctionExit p i) L.\\ kill block) ++ gen block
    where block = getStat' i (programToStat p)

