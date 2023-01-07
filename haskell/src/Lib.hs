module Lib
    ( someFunc
    ) where

import Parts ( donitz ) 
import Diagram ( drawEnigma, defaultShape )
--import Diagrams.Backend.SVG.CmdLine ( mainWith )
import Diagrams.Backend.Cairo.CmdLine (mainWith)
import Control.Monad.Reader (runReader)
import Enigma
import Language (EnglishLetter, LetterOrdinal(..))
import Data.TypeLits (KnownNat)
import Parts
import Cartridge 
import Enigma
import Rotor
import Control.Monad.State.Strict (evalState, runState)
import Cipher
--import Bombe.Tracker (doTrackSpecsOf)

someFunc :: IO ()
someFunc = do
    --doTrackSpecsOf donitz
    let e = donitz -- :: (KnownNat 26) => EnigmaState EnglishLetter
    let drawing = drawEnigma e 0

    mainWith $ runReader drawing defaultShape
    --mainWith $ maybe mempty (\x -> runReader (drawEnigma x 0) defaultShape) (Just donitz)