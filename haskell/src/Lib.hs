module Lib
    ( someFunc
    ) where

import Enigma
import Parts
import Rotor 
import Transform
import Control.Monad.State.Strict
import Language
import Cipher
import Diagram
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import Control.Monad.Writer.Strict

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

traceenc = 
    do --Maybe 
        e <- e6
        plaintext  <- Just B
        let cipherState = tracedEncryptEnigma plaintext  --Maybe (Enigma)
        return $ (runState cipherState e :: (Writer [Letter] Letter, EnigmaState Letter)) 


someFunc :: IO ()
someFunc = do
    (Rotor tr@(Transform t) _) <- return $ im6 
    
    print (show (map (encrypt tr) [A, B, C, D, Language.E, F]))
    e <- return $ e6 -- Maybe EnigmaState
    --figure <- return $ 
        --identity 
        --do 
      --      e 

    print traceenc
    mainWith $ maybe (mempty) (\x -> drawEnigma x 0) e
    --mainWith $ drawRotor i [0]
    --mainWith $ drawEnigma e 2