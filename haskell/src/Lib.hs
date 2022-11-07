module Lib
    ( someFunc
    ) where

import Enigma
import Parts
import Control.Monad.State

someFunc :: IO ()
someFunc = do
    let e = mkEnigma donitzPlugboard donitzRotors thinReflectorC donitzRingSetting donitzRotorPosition
    let plaintext = [E,K,F,F,O,U,Z,L]
    let ciphertext = runState (encryptText plaintext) e
    print ciphertext
