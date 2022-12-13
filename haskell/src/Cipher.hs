module Cipher (
Cipher(..),
PolyalphabeticCipher(..),
) where






-----------------------------------------------------------------------
--                          Ciphers                                  --
-----------------------------------------------------------------------
class (Enum c) => Cipherable c

class Cipher c where
    encrypt ::(Ord e, Enum e) => c e -> e -> e
    decrypt ::(Ord e, Enum e) => c e -> e -> e
    letters :: c e -> Int
    encryptEntirety :: (Ord e, Enum e) => c e -> [e]
    encryptEntirety c = map (encrypt c . toEnum) [0..(letters c-1)]
    decryptEntirety :: (Ord e, Enum e) => c e -> [e]
    decryptEntirety c = map (encrypt c . toEnum) [0..(letters c-1)]



class (Cipher c) => MonadCipher c where
    monadicEncrypt :: (Ord e, Enum e, Monad m) => c e -> m e -> m e
    monadicDecrypt :: (Ord e, Enum e, Monad m) => c e -> m e -> m e
    monadicEncrypt cipher c = do encrypt cipher <$> c
    monadicDecrypt cipher c = do decrypt cipher <$> c

class (Cipher c) => MonoidCipher c where
    monoidEncrypt :: (Ord e, Monoid (m e)) => c e -> m e -> m e
    monoidDecrypt :: (Ord e, Monoid (m e)) => c e -> m e -> m e
    --monoidEncrypt cipher p = (encrypt <> mempty
    --monoidDecrypt cipher p = decrypt <> mempty --hmmmmmm

class PolyalphabeticCipher c where
    polyEncrypt :: (Enum e) => c e e -> c e es
    polyDecrypt :: (Enum e) => c e e -> c e e

class (PolyalphabeticCipher c) => PolyalphabeticMonoidCipher c where
    polyMonoidEncrypt :: (Monoid m) => c m e -> c m e
    polyMonoidDecrypt :: (Monoid m) => c m e -> c m e

    --encryptText :: (Traversable t, Monad (c (t e))) => c (t e) l -> c (t e) l 
    --encryptText text = mapM polyEncrypt text

--class (PolyalphabeticCipher c, Monad m) => MonadicPolyalphabeticCipher c m where
--    monadicPolyEncrypt :: (Language l, Enum e, Monad m) => c (m e) l -> c (m e) l
--    monadicPolyEncrypt cipher = 


--        do
--         mEncrypted <- polyEncrypt


        --take our state, unlift value monad, encrypt..
        --for example we want it to handle list
        -- should encrypt each element in same state, but how to agree on arrived state???
        -- state should not depend on input, but haskell does not know that...
        --could get undeterministic in that encrypt of list returns LIST of encrypts (ie several states...)

    --polyDecrypt :: (Language l, Enum e) => c e l -> c e l

--TODO
-- separate polyciphers which only depend on position in plaintext, not on the plaintext itself!
-- ie no matter what input is given to it, the next state is always the same(from current position...)
-- okay okay, given a enigma, the trajectory is ALWAYS predefined from the current state.
-- is enigma ACTUALLY a stateful monad???

