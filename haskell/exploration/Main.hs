module Main(main) where

import Lib
import Parts
import Bombe.Wiring
import Enigma hiding(initialize)
import Language
import Control.Monad.State.Strict
import Diagram ( drawEnigma, defaultShape )
--import Diagrams.Backend.SVG.CmdLine ( mainWith )
import Diagrams.Backend.Cairo.CmdLine

import Bombe.Wiring
import Bombe.WiringDiagram (drawWiresDefault)

main :: IO ()
main = do 
    putStrLn "We start with a simple4 enigma"
    let 
        enigma = simple6
        wiring = initialize 6 :: MatrixWiring
    --print enigma 
    --putStrLn "And an initialized wiring"
    --print wiring 
    --putStrLn "connect 1"
    --putStrLn $ prettyPrintMatrix (getMatrix (evalState (Enigma.connect wiring A B) simple6)) 6
    mainWith $ drawWiresDefault (evalState (Enigma.connect wiring A B) simple6)