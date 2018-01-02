module Main where

import Automaton

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    let i = addState $ addState $ emptyInter
    let j = setTransitionsI i [(0, 1, 1), (0, 2, 1), (1, 0, 0)]
    let k = setLensI (setSlinksI j [(1, 0)]) [(1, 2), (0, 1)]

    dotifyToFile "/tmp/sa.dot" k