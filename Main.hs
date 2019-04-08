#!/usr/bin/env runhaskell
import Data.Char (isAlpha)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

data DFA = DFA {state :: State, output :: String}
data Identifier = A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | 
    B | D | E | F1 | F2 | G | H | I | J1 | J2
    deriving (Eq, Show)
data State = State Identifier [Edge]
data Edge = Edge {transition :: Char -> String -> String, condition :: Char -> Bool, dest :: Identifier}

iEdge = Edge $ curry snd
appendFname ch = (++ [ch])
fEdge = Edge appendFname

dfa = [
    State A1 [iEdge (== '#') A2], State A2 [iEdge (== 'i') A3], 
    State A3 [iEdge (== 'n') A4], State A4 [iEdge (== 'c') A5], 
    State A5 [iEdge (== 'l') A6], State A6 [iEdge (== 'u') A7], 
    State A7 [iEdge (== 'd') A8], State A8 [iEdge (== 'e') B], 
    State B [iEdge (== ' ') B, iEdge (== '"') D, iEdge (== '<') H],
    State D [fEdge isAlpha E], 
    State E [fEdge isAlpha E, iEdge (== '"') G, fEdge (== '.') F1],
    State F1 [fEdge (== 'h') F2], State F2 [iEdge (== '"') G],
    State H [fEdge isAlpha I],
    State I [fEdge isAlpha I, fEdge (== '.') J1, iEdge (== '>') G],
    State J1 [fEdge (== 'h') J2], State J2 [iEdge (== '>') G],
    State G []
    ]

dfaErr identifier = error $ "Unknown state:" ++ show identifier

getState :: Identifier -> State
getState identifier = fromMaybe (dfaErr identifier) $ find check dfa
    where check (State a _) = a == identifier

inputErr s i = 
    error $ "Wrong input symbol: " ++ show i ++ ", for state: " ++ show s
jump :: DFA -> Char -> DFA
jump (DFA (State s es) out) input = DFA (getState $ dest edge) nextOutput
    where
        edge :: Edge
        edge = fromMaybe err $ find (($ input) . condition) es
        nextOutput = transition edge input out
        err = inputErr s input

run :: DFA -> String -> DFA
run dfa [] = dfa
run dfa (x:xs) = run (jump dfa x) xs

runAutomaton = output . run (DFA (head dfa) "")

main = print . map runAutomaton . lines =<< (readFile . head) =<< getArgs
