module Cipher (
Cipher(..),
PolyalphabeticCipher(..),
) where

import Language (Language)

-----------------------------------------------------------------------
--                          Ciphers                                  --
-----------------------------------------------------------------------
class Cipher c where
    encrypt :: (Language l, Enum e) => c l -> e -> e
    decrypt :: (Language l, Enum e) => c l -> e -> e

class PolyalphabeticCipher c where
    polyEncrypt :: (Language l, Enum e) => c e l -> c e l
    polyDecrypt :: (Language l, Enum e) => c e l -> c e l
    --encryptText :: (Traversable t, Monad (c (t e))) => c (t e) l -> c (t e) l 
    --encryptText text = mapM polyEncrypt text