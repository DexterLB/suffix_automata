module Main where

import Automaton

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
    let i = addState $ addState $ emptyInter
    let j = setTransitionsI i [(0, [('a', 1), ('b', 1)]), (1, [('c', 0)])]
    let k = setLensI (setSlinksI j [(1, 0)]) [(1, 2), (0, 1)]

    dotifyToFile "/tmp/sa.dot" k