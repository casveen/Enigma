module Rotor (
Rotor(..)
) where
import Transform (Transform(..))
import Language (Language(..), HasLanguage(..))
import Cipher (Cipher(..))
----------------------------------------------
--              ROTOR                       --
----------------------------------------------    
data Rotor l = Rotor (Transform l) [Int] | Reflector (Transform l) [Int] deriving(Eq)
instance (Language l) => Show (Rotor l) where
    show (Rotor t ns) = "|" ++ show t ++ "| - "
                     ++ show (map (pad0 2) ns)
    show (Reflector t ns) = "<" ++ show t ++ "> - "
                     ++ show (map (pad0 2) ns)
instance HasLanguage Rotor where
    getLanguage (Rotor t _) = getLanguage t
    getLanguage (Reflector t _) = getLanguage t
instance Cipher Rotor where
    encrypt (Rotor t _)     = encrypt t
    encrypt (Reflector t _) = encrypt t
    decrypt (Rotor t _)     = decrypt t
    decrypt (Reflector t _) = decrypt t

----------------------------------------------
--          HELPER FUNCTIONS                --
----------------------------------------------  
pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x

--fpow 0 f = id
--fpow n f = f . fpow (n-1) f