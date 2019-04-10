#!/usr/bin/env runhaskell
import Data.Char (isAlpha)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

data Identifier = A1 | A2 | A3 | A4 | A5 | A6 | A7 | A8 | 
    B | D | E | F1 | F2 | G | H | I | J1 | J2
    deriving (Eq, Show)
data State = State Identifier [Edge]
data Edge = Edge {condition :: Char -> Bool, dest :: Identifier}

dfa = [
    State A1 [Edge (== '#') A2], State A2 [Edge (== 'i') A3], 
    State A3 [Edge (== 'n') A4], State A4 [Edge (== 'c') A5], 
    State A5 [Edge (== 'l') A6], State A6 [Edge (== 'u') A7], 
    State A7 [Edge (== 'd') A8], State A8 [Edge (== 'e') B], 
    State B [Edge (== ' ') B, Edge (== '"') D, Edge (== '<') H],
    State D [Edge isAlpha E], 
    State E [Edge isAlpha E, Edge (== '"') G, Edge (== '.') F1],
    State F1 [Edge (== 'h') F2], State F2 [Edge (== '"') G],
    State H [Edge isAlpha I],
    State I [Edge isAlpha I, Edge (== '.') J1, Edge (== '>') G],
    State J1 [Edge (== 'h') J2], State J2 [Edge (== '>') G],
    State G []
    ]
destFilename = [E, F1, F2, I, J1, J2]

getState :: Identifier -> Maybe State
getState identifier = find check dfa
    where check (State a _) = a == identifier

dfaErr identifier = error $ "Unknown state:" ++ show identifier
inputErr s i = 
    error $ "Wrong input symbol: " ++ show i ++ ", for state: " ++ show s
nextState :: Edge -> State
nextState (Edge _ st) = fromMaybe (dfaErr st) $ getState st

jump :: State -> Char -> State
jump (State s es) input = nextState $ fromMaybe err edge
    where
        edge = find (($ input) . condition) es
        err = inputErr s input


run state fname []  = fname 
run state fname (x:xs)  = getFname dest ++ run dest fname xs
    where
        dest = jump state x
        getFname (State dest _) 
            | dest `elem` destFilename = fname ++ [x]
            | otherwise = fname

runAutomaton = run (head dfa) ""

main = print . map runAutomaton . lines =<< (readFile . head) =<< getArgs
