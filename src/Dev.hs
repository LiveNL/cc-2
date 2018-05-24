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

parse :: String -> IO Stat'
parse programName = do
  let fileName = "../examples/"++programName++".c"
  content <- readFile fileName
  return . programToStat . happy . alex $ content




programToStat :: Program -> Stat'
programToStat (Program _ s) = toStatPrime s 0


toStatPrime (Skip) i = Skip' i 
toStatPrime (IAssign a b) i = IAssign' i a b
toStatPrime (BAssign a b) i = BAssign' i a b
toStatPrime (IfThenElse c s1 s2) i = IfThenElse' i c (toStatPrime s1 (i+1)) (toStatPrime s2 (i+1))
toStatPrime (While c s) i = While' i c (toStatPrime s (i+1))
--toStatPrime (Call  s) i = While' i c (toStatPrime s (i+1))
--toStatPrime (IAssign c s) i = While' i c (toStatPrime s (i+1))
-- toStatPrime (BAssign c s) i = While' i c (toStatPrime s (i+1))
toStatPrime (Seq s1 s2) i = Seq' (toStatPrime s1 (i+1))  (toStatPrime s2 (i+1))
toStatPrime _ i = Skip' i
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
final (Seq' s1 _) = final (s1)

flow:: Stat' -> [(Int, Int)]
flow (Skip' i) = []
flow (IAssign' i _ _ ) = []
flow (BAssign' i _ _ ) = []
flow (IfThenElse' i _ s1 s2) = flow s1 ++ flow s2 ++ [(i, initStat s1),(i, initStat s2)]
flow (While' i c s) = flow s ++ [(i,initStat s)] ++ map (\x -> (x,i)) (final s)
flow (Seq' s1 s2) = flow s1 ++ flow s2 ++  map (\x -> (x,initStat s2)) (final s1)

killLV(IAssign' i n _)= [n]
killLV(Skip' _)= []
killLV _ = []

gen (IAssign' i n _) = []
gen (Skip' i) = []
