module Lib
    ( someFunc
    ) where

import Parts ( donitz ) 
import Diagram ( drawEnigma, defaultShape )
import Diagrams.Backend.SVG.CmdLine ( mainWith )
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

    print "ha!"

    print "we are trying to recreate a stck overflow in the recryption of a sequence of letters!"

    let rotors = [ii, v, ii] :: [Rotor EnglishLetter]
    let positions = [25,23,07]
    let reflector = thinReflectorC :: Rotor EnglishLetter
    let cartridge = Cartridge rotors reflector positions
    let e = Enigma identityPlugboard cartridge
{-
with
Enigma()
ID  POS                                 NOTCH
          ABCDEFGHIJKLMNOPQRSTUVWXYZ
P:        ABCDEFGHIJKLMNOPQRSTUVWXYZ
R0: [24] |AJPCZWRLFBDKOTYUQGENHXMIVS| - ["04"]
R1: [23] |QCYLXWENFTZOSMVJUDKGIARPHB| - ["25"]
R2: [07] |AJPCZWRLFBDKOTYUQGENHXMIVS| - ["04"]
Re:      <RDOBJNTKVEHMLFCWZAXGYIPSUQ> - []
---------------------------------------------- 
-}

    let message = [B]-- , U]

    print (show rotors)
    print (show e)
    print (encrypt e (toEnum . fromEnum $ E))
    print (encrypt e (toEnum . fromEnum $ U))
    
    --let renum = map reEnum message 
    --print renum

    
    --let enc = evalState (encrypt renum) e

    print (runState (do return 0) e)
    --print (runState (do step) e)
    --print (runState (do {enc 0}) e)
    print "we try with just the cartridge!"
    print (stepCartridge cartridge)
    --print (encrypt (stepCartridge cartridge) 0)
    --print enc
    --let renc =  evalState (encryptText enc) e
    --print renc

    --mainWith $ runReader drawing defaultShape
    --mainWith $ maybe mempty (\x -> runReader (drawEnigma x 0) defaultShape) (Just donitz)