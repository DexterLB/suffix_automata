{-# LANGUAGE NamedFieldPuns #-}

module Automaton
    where

import Debug.Trace (traceShowId)

import Data.Char (ord, chr)

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as VB


import qualified Data.List as L

alphabetSize    = (ord 'z') - (ord 'a') + 1
alphabetOffset  = (ord 'a')
lastLetter      = alphabetSize - 1

data Inter = Inter {
    transitions     :: VB.Vector Transitions
  , slinks          :: VU.Vector Int
  , lens            :: VU.Vector Int
  , numStates       :: Int
  , epsilonState    :: Int
  , wordState       :: Int
}

data Transitions = Transitions {
    withs           :: VU.Vector Char
  , tos             :: VU.Vector Int
}

emptyInter :: Inter
emptyInter = Inter {
        transitions     = VB.empty
      , slinks          = VU.empty
      , lens            = VU.empty
      , numStates       = 0
      , epsilonState    = -1
      , wordState       = -1
    }

emptyTransitions :: Transitions
emptyTransitions = Transitions {
        withs = VU.empty
      , tos   = VU.empty
    }

addState :: Inter -> Inter
addState i =
    let (Inter { transitions, slinks, lens, numStates}) = i in
        i {
            transitions = VB.snoc transitions emptyTransitions
          , lens        = VU.snoc lens 0
          , slinks      = VU.snoc slinks (-1)
          , numStates   = numStates + 1
        }

setStateTransition :: (Char, Int) -> Transitions -> Transitions
setStateTransition (with, to) (Transitions {withs, tos})
    | (Just i) <- index, withs VU.! i == with = Transitions {
            withs = withs
          , tos   = tos VU.// [(i, to)]
        }
    | (Just i) <- index = Transitions {
            withs = insertVU i with withs
          , tos   = insertVU i   to   tos
        }
    | otherwise = Transitions {
            withs = VU.snoc withs with
          , tos   = VU.snoc   tos   to
        }
    where
        index = VU.findIndex (>= with) withs

setStateTransitions :: Transitions -- ^ Transitions
                    -> [(Char, Int)]
                    -> Transitions
setStateTransitions = foldr setStateTransition

setTransitions :: VB.Vector Transitions
               -> [(Int, [(Char, Int)])]
               -> VB.Vector Transitions
setTransitions transitions updates = transitions VB.//
    (map (\(from, inner) -> (from, setStateTransitions (transitions VB.! from) inner)) updates)

setTransitionsI :: Inter -> [(Int, [(Char, Int)])] -> Inter
setTransitionsI i l = i { transitions = setTransitions (transitions i) l }

setSlinks :: VU.Vector Int -> [(Int, Int)] -> VU.Vector Int
setSlinks = (VU.//)

setSlinksI :: Inter -> [(Int, Int)] -> Inter
setSlinksI i l = i { slinks = setSlinks (slinks i) l }

setLens :: VU.Vector Int -> [(Int, Int)] -> VU.Vector Int
setLens = (VU.//)

setLensI :: Inter -> [(Int, Int)] -> Inter
setLensI i l = i { lens = setLens (lens i) l }


transition :: VB.Vector Transitions
           -> Int           -- ^ State index
           -> Char          -- ^ Transition index
           -> Maybe Int     -- ^ State which the transition points to
transition transitions from with = transitionFrom (transitions VB.! from) with

transitionFrom :: Transitions -> Char -> Maybe Int
transitionFrom (Transitions { withs, tos }) c
    = (tos VU.!) <$> (VU.elemIndex c withs)


decodeLetter :: Char -> Char
decodeLetter = id

encodeLetter :: Char -> Char
encodeLetter = id

transitionsFrom :: VB.Vector Transitions -> Int -> [(Int, Char, Int)]
transitionsFrom transitions from =
    map (\(with, to) -> (from, with, to)) (zipTransitions (transitions VB.! from))

zipTransitions :: Transitions -> [(Char, Int)]
zipTransitions (Transitions { withs, tos }) = zip (VU.toList withs) (VU.toList tos)

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
            (show i) ++ " [label=\"" ++ (show i) ++ " len=" ++ (show (lens VU.! i)) ++ "\"];"
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
                    (i, "color=\"#40875b\"", (slinks VU.! i))
                    : map
                        (\(from, with, to) ->
                            (from, transitionLabel lens (from, with, to), to)
                        )
                        (transitionsFrom transitions i)
                )
        )

transitionLabel :: VU.Vector Int -> (Int, Char, Int) -> String
transitionLabel lens (from, with, to)
    | lenDiff <= 1 = label
    | otherwise    = label ++ ",style=dotted"
    where
        label   = "label=\"" ++ [(decodeLetter with)]++ "\""
        lenDiff = abs (len1 - len2)
        len1    = lens VU.! from
        len2    = lens VU.! to

dotifyToFile :: FilePath -> Inter -> IO ()
dotifyToFile f i = writeFile f (dotify i)


-- utils

insertVU :: (VU.Unbox a) => Int -> a -> VU.Vector a -> VU.Vector a
insertVU index elem v = (VU.snoc (VU.take index v) elem) VU.++ (VU.drop index v)
