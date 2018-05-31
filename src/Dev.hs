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

--slv = undefined
--cp  = undefined

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
parse :: String -> IO [(Int, [String])]
parse programName = do
  let programName = "college"
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  let (freshLabel, t) = sem_Program (happy . alex $ content) 1
  let f = getFlow t
  return (slv t)

getFlow (Program' p s) = flow s

-- write in AG
initStat :: Stat' -> Int
initStat (Skip' i)             = i
initStat (IAssign' i _ _)      = i
initStat (BAssign' i _ _)      = i
initStat (IfThenElse' i _ _ _) = i
initStat (While' i _ _)        = i
initStat (Seq' s1 _)           = initStat s1

final :: Stat' -> [Int]
final (Skip' i)               = [i]
final (IAssign' i _ _ )       = [i]
final (BAssign' i _ _ )       = [i]
final (IfThenElse' i _ s1 s2) = final s1 ++ final s2
final (While' i _ _)          = [i]
final (Seq' _ s2)             = final s2

flow :: Stat' -> [(Int, Int)]
flow (Skip' i)               = []
flow (IAssign' i _ _ )       = []
flow (BAssign' i _ _ )       = []
flow (IfThenElse' i _ s1 s2) = flow s1 ++ flow s2 ++ [(i, initStat s1),(i, initStat s2)]
flow (While' i c s)          = flow s ++ [(i,initStat s)] ++ map (\x -> (x,i)) (final s)
flow (Seq' s1 s2)            = flow s1 ++ flow s2 ++ map (\x -> (x,initStat s2)) (final s1)

reversedFlow :: Stat' -> [(Int, Int)]
reversedFlow s = map (\(x,y) -> (y,x)) (flow s)

-- complete lattice:
-- misschien moeten de statements uit de toegevoegde onderdelen nog wel en alleen label en cnd worden
programToStat :: Program' -> [Stat']
programToStat (Program' _ s) = statToStatList s

statToStatList (Skip' i)               = [Skip' i]
statToStatList (IAssign' i a b)        = [IAssign' i a b]
statToStatList (BAssign' i a b)        = [BAssign' i a b]
statToStatList (IfThenElse' i c s1 s2) = [IfThenElse' i c s1 s2] ++ (statToStatList s1) ++ (statToStatList s2)
statToStatList (While' i c s)          = [While' i c s] ++ (statToStatList s)
statToStatList (Seq' s1 s2)            = (statToStatList s1)  ++ (statToStatList s2)
statToStatList _                       = [Skip' 0]

allVars :: Stat' -> [String]
allVars (Skip' _)          = []
allVars (IAssign' _ _ val) = freeVarsI val
allVars (BAssign' _ _ val) = freeVarsB val
allVars _                  = []

getLabel (Skip' i)               = i
getLabel (IAssign' i _ _ )       = i
getLabel (BAssign' i _ _ )       = i
getLabel (IfThenElse' i _ s1 s2) = i
getLabel (While' i _ s)          = i
getLabel (Seq' s1 _)             = getLabel s1

getLabels (Skip' i)               = [i]
getLabels (IAssign' i _ _ )       = [i]
getLabels (BAssign' i _ _ )       = [i]
getLabels (IfThenElse' i _ s1 s2) = [i] ++ getLabels s1 ++ getLabels s2
getLabels (While' i _ s)          = [i] ++ (getLabels s)
getLabels (Seq' s1 s2)            = (getLabels s1) ++ (getLabels s2)


--            let lattice       = allVars s -- moet nog

--cp p@(Program' _ s) flow =
----            let lattice       = allVars s -- moet nog
--            let transitions   = flow s
--                extremalLabel = [initStat s]
--                extremalValue = map (\x -> (x, [0])) (map getLabel (allVars2 s))
----                bottomList    = map (\x -> (x, [])) ((getLabel s) L.\\ extremalLabel) -- A init p96
----                extremalList  = map (\x -> (x, extremalValue)) extremalLabel
--                a             = extremalValue -- bottomList ++ extremalList
--             in mfpIteration transitions a s p  -- S = W from book/slides
--

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

--mfpIteration :: [(Int, Int)] -> [(Int, [String])] -> Stat' -> Program' -> [(Int, [String])]
--mfpIteration [] a s p    = a
--mfpIteration ((l1,l2) : xs) a s p = let a1 = concatMap (\(x1,x2) -> if x1 == l1 then x2 else []) a  -- allVars (getStat l1 s)
--                                        a2 = concatMap (\(x1,x2) -> if x1 == l2 then x2 else []) a
--                                        x1 = (a1 L.\\ kill (getStat l1 s)) ++ gen (getStat l1 s)
--                                        superset = getSuperset a2 x1
--                                        newA2 = if not superset then (l1, a2 ++ x1) else (l2, a2)
--                                        removeA2 = filter (\(x, y) -> l2 /= x) a
--                                        newA = a ++ [newA2]
--                                    in  mfpIteration xs newA s p
--
slv p@(Program' _ s) =
            let transitions   = reversedFlow s
                extremalLabels = final s
                extremalValue = []
                bottomList    = map (\x -> (x, [])) ((getLabels s) L.\\ extremalLabels)
                extremalList  = map (\x -> (x, [])) extremalLabels
                a             = bottomList ++ extremalList
             in mfp (statToStatList s) transitions extremalLabels extremalValue slvTransferFunction [] True True s -- cleanup s/slist

slvTransferFunction a1 label stms = (a1 L.\\ kill (getStat label stms)) ++ gen (getStat label stms)

mfp stms transitions extremalLabels extremalValue transferFunctions bottom setType ion s = let a = mfpInit s extremalLabels extremalValue bottom
                                                                                               w = transitions
                                                                                           in mfpIteration a w transferFunctions s setType ion

mfpInit statements extremalLabels extremalValue bottom = setA statements
  where setA stmt = if (getLabel stmt) `elem` extremalLabels
                    then map (\x -> (x, extremalValue)) extremalLabels
                    else map (\x -> (x, [])) ((getLabels stmt) L.\\ extremalLabels)

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

isSuperSet :: [String] -> [String] -> Bool
isSuperSet _ [] = True
isSuperSet x (a:as) | a `elem` x = isSuperSet x as
                    | otherwise = False


-- x1 = cpTransferFunction (getStat l1 s)
--
--data State = Top | Bottom | Num Int | VarX String
--
--type Sigma = [(String, State)]
--
--cpTransferFunction :: Stat' -> Sigma -> [String]
--cpTransferFunction (Skip' _) s         = s
--cpTransferFunction (IAssign' _ x a) s | null s =  []
--  | otherwise = s ++ (x, (acp a s))
--
--cpTransferFunction (BAssign' _ _ _) s = s
--cpTransferFunction _                  = []
--
--acp (IConst v) s = Num v
--acp (Var n) s = Num (snd (head ((filter (\(x1,x2) -> x1 == n)) s)))
--acp (Plus a1 a2) = Num (+ (toVal (acp a1)) (toVal (acp a2)))
--acp (Minus a1 a2) = Num (- (toVal (acp a1)) (toVal (acp a2)))
--acp (Times a1 a2) = Num (* (toVal (acp a1)) (toVal (acp a2)))
--acp (Divide a1 a2) = Num (div (toVal (acp a1)) (toVal (acp a2)))
--
--toVal (Num x) = x

getSuperset :: [String] -> [String] -> Bool
getSuperset _ [] = True
getSuperset x (a : as) | a `elem` x = getSuperset x as
                       | otherwise = False

-- AUXILARY FUNCTIONS FOR LIVE VARIABLES IN MFP
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

-- BASIC LIVE VARIABLES IMPLEMENTATION
-- tranfer functions live variable
transferFunctionExit :: Program' -> Int -> [String]
transferFunctionExit p@(Program' _ s) i | i `elem` (final s) = []
                                        | otherwise = concatMap (\(ll1, ll2) -> transferFunctionEntry p ll1) (filter (\(l1, l2) -> l2 == i) (reversedFlow s))

transferFunctionEntry :: Program' -> Int -> [String]
transferFunctionEntry p@(Program' _ s) i = ((transferFunctionExit p i) L.\\ kill block) ++ gen block
    where block = getStat' i (programToStat p)

