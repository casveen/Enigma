{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Cipher (
Cipher(..),
TraceableCipher(..), 
PolyalphabeticCipher(..),
logTrace
) where
import Control.Monad.Writer.Strict (Writer, writer)

-----------------------------------------------------------------------
--                          Ciphers                                  --
-----------------------------------------------------------------------
class Cipher c where
    encrypt ::(Ord e, Enum e) => c e -> e -> e
    decrypt ::(Ord e, Enum e) => c e -> e -> e
    letters :: c e -> Int
    encryptEntirety :: (Ord e, Enum e) => c e -> [e]
    encryptEntirety c = map (encrypt c . toEnum) [0..(letters c-1)]
    decryptEntirety :: (Ord e, Enum e) => c e -> [e]
    decryptEntirety c = map (encrypt c . toEnum) [0..(letters c-1)]

logTrace :: (Monoid (w e), Monad w) => e -> Writer (w e) e
logTrace t = writer (t, return t)

class (Cipher c) => TraceableCipher c where
    tracedEncrypt :: (Enum e, Ord e, Monoid (w e), Monad w) => c e -> e -> Writer (w e) e

class (Cipher c) => MonadCipher c where
    monadicEncrypt :: (Ord e, Enum e, Monad m) => c e -> m e -> m e
    monadicDecrypt :: (Ord e, Enum e, Monad m) => c e -> m e -> m e
    monadicEncrypt cipher c = do encrypt cipher <$> c
    monadicDecrypt cipher c = do decrypt cipher <$> c

class (Cipher c) => MonoidCipher c where
    monoidEncrypt :: (Ord e, Monoid (m e)) => c e -> m e -> m e
    monoidDecrypt :: (Ord e, Monoid (m e)) => c e -> m e -> m e

class PolyalphabeticCipher c where
    polyEncrypt :: (Enum e) => c e e -> c e es
    polyDecrypt :: (Enum e) => c e e -> c e e

class (PolyalphabeticCipher c) => PolyalphabeticMonoidCipher c where
    polyMonoidEncrypt :: (Monoid m) => c m e -> c m e
    polyMonoidDecrypt :: (Monoid m) => c m e -> c m e
