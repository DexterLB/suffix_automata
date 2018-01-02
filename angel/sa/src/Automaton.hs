{-# LANGUAGE NamedFieldPuns #-}

module Automaton
    where

import Debug.Trace (traceShowId)

import Data.Char (ord, chr)

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

import qualified Data.List as L

alphabetSize    = (ord 'z') - (ord 'a') + 1
alphabetOffset  = (ord 'a')
lastLetter      = alphabetSize - 1

data Inter = Inter {
    transitions :: Vector Int
  , slinks      :: Vector Int
  , lens        :: Vector Int
  , numStates   :: Int
}

emptyInter :: Inter
emptyInter = Inter {
        transitions = V.empty
      , slinks      = V.empty
      , lens        = V.empty
      , numStates   = 0
    }

addState :: Inter -> Inter
addState (Inter { transitions, slinks, lens, numStates})
    = Inter {
        transitions = transitions V.++ (V.replicate alphabetSize (-1))
      , lens        = V.snoc lens 0
      , slinks      = V.snoc slinks (-1)
      , numStates   = numStates + 1
    }

setTransitions :: Vector Int         -- ^ Transitions
               -> [(Int, Int, Int)]  -- ^ From, With, To
               -> Vector Int

setTransitions transitions updates = transitions V.//
    (map (\(from, with, to) -> (from * alphabetSize + with, to)) updates)

setTransitionsI :: Inter -> [(Int, Int, Int)] -> Inter
setTransitionsI i l = i { transitions = setTransitions (transitions i) l }

setSlinks :: Vector Int -> [(Int, Int)] -> Vector Int
setSlinks = (V.//)

setSlinksI :: Inter -> [(Int, Int)] -> Inter
setSlinksI i l = i { slinks = setSlinks (slinks i) l }

setLens :: Vector Int -> [(Int, Int)] -> Vector Int
setLens = (V.//)

setLensI :: Inter -> [(Int, Int)] -> Inter
setLensI i l = i { lens = setLens (lens i) l }


transition :: Vector Int
           -> Int   -- ^ State index
           -> Int   -- ^ Transition index
           -> Int   -- ^ State which the transition points to
transition transitions from with
    = transitions V.! (from * alphabetSize + with)


decodeLetter :: Int -> Char
decodeLetter i = chr $ i + alphabetOffset

encodeLetter :: Char -> Int
encodeLetter c = (ord c) - alphabetOffset

transitionsFrom :: Vector Int -> Int -> [(Int, Int, Int)]
transitionsFrom transitions from =
    map
        (\with -> (from, with, transition transitions from with))
        [0..lastLetter]

-- Prints
dotify :: Inter -> String
dotify inter = "digraph d {\n" ++ (L.intercalate "\n" lines) ++ "\n}\n"
    where
        lines = map ("    " ++) $ concat $ map (dotifyState inter) [0..(numStates inter - 1)]

dotifyState :: Inter -> Int -> [String]
dotifyState (Inter {transitions, slinks, lens, numStates}) i
    | i == numStates = []
    | otherwise
        = [
            (show i) ++ " [label=\"" ++ (show i) ++ " len=" ++ (show (lens V.! i)) ++ "\"];"
        ]
        ++ (
            map
                (\(from, info, to)
                    -> (show from)
                    ++ " -> "
                    ++ (show to)
                    ++ " ["
                    ++ info
                    ++ "];")
            $ filter (\(_, _, to) -> to /= (-1))
                (
                    (i, "color=\"#40875b\"", (slinks V.! i))
                    : map
                        (\(from, with, to) ->
                            (from, transitionLabel lens (from, with, to), to)
                        )
                        (transitionsFrom transitions i)
                )
        )

transitionLabel :: Vector Int -> (Int, Int, Int) -> String
transitionLabel lens (from, with, to)
    | lenDiff <= 1 = label
    | otherwise    = label ++ ",style=dotted"
    where
        label   = "label=\"" ++ [(decodeLetter with)]++ "\""
        lenDiff = abs (len1 - len2)
        len1    = lens V.! from
        len2    = lens V.! to

dotifyToFile :: FilePath -> Inter -> IO ()
dotifyToFile f i = writeFile f (dotify i)