module Lib
    ( someFunc
    ) where

import Enigma
import Parts
import Control.Monad.State
import Language

cipher = 
    do --Maybe 
        e <- mkEnigma 
            donitzPlugboard 
            donitzRotors 
            thinReflectorC 
            donitzRingSetting 
            donitzRotorPosition -- Maybe EnigmaState
        plaintext  <- safelyReadLetters "EKFFOUZL" --Maybe Letters
        let cipherState = encryptText plaintext --Maybe (Enigma)
        return $ runState cipherState e

someFunc = do 
    print cipher
            