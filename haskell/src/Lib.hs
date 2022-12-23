module Lib
    ( someFunc
    ) where

import Parts 
import Cartridge
import Plugboard 
import Language
import Enigma
--( donitz, simple4, simple6)
import Enigma (stepEnigma, EnigmaState)
import Diagram ( drawEnigma, defaultShape )
import Diagrams.Backend.SVG.CmdLine ( mainWith )
import Control.Monad.Reader (runReader)
import Data.List (iterate')
import Diagrams (iterateN)
import Bombe.Tracker (trackManually, doTrackSpecsOf)
import Control.Monad.State.Strict (evalState)
import Language (Letter)

simple42 :: EnigmaState Letter
simple42 =     let 
        cartridge = Cartridge [im, iim, iiim] ukwm [3,0,1]
        plugboard = mkPlugboard [(A,A),(B,B),(C,C),(D,D)]
    in 
        Enigma plugboard cartridge

someFunc :: IO ()
someFunc = do
    --print $ trackManually simple42
    --doTrackSpecsOf simple4
    --doTrackSpecsOf simple6
    doTrackSpecsOf donitz
    mainWith $ runReader (drawEnigma donitz 0) defaultShape
    --mainWith $ maybe mempty (\x -> runReader (drawEnigma x 0) defaultShape) (Just donitz)