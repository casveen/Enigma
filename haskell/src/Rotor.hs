module Rotor (
Rotor(..)
) where
import Transform (Transform(..))
import Cipher (Cipher(..))

----------------------------------------------
--              ROTOR                       --
----------------------------------------------    
data Rotor e = Rotor (Transform e) [Int] | Reflector (Transform e) [Int] deriving(Eq)

-----------------------------------------------
--             INSTANCES                     --
-----------------------------------------------
instance (Show e) => Show (Rotor e) where
    show (Rotor t ns) = "|" ++ show t ++ "| - "
                     ++ show (map (pad0 2) ns)
    show (Reflector t ns) = "<" ++ show t ++ "> - "
                     ++ show (map (pad0 2) ns)
instance Cipher Rotor where
    encrypt (Rotor t _)     = encrypt t
    encrypt (Reflector t _) = encrypt t
    decrypt (Rotor t _)     = decrypt t
    decrypt (Reflector t _) = decrypt t
    letters (Rotor t _)     = letters t
    letters (Reflector t _) = letters t

    
----------------------------------------------
--          HELPER FUNCTIONS                --
----------------------------------------------  
pad0 :: Show p => Int -> p -> [Char]
pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x