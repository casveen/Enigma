module Rotor (
Rotor(..),
getTransform,
getNotches,
getName,
displace
) where
import Transform (Transform(..))
import Cipher (Cipher(..))

----------------------------------------------
--              ROTOR                       --
----------------------------------------------    
data Rotor e = Rotor (Transform e) [e] | Reflector (Transform e) [e] | NamedRotor (Rotor e) String deriving(Eq)

getTransform :: Rotor e -> Transform e
getTransform (Rotor t _) = t
getTransform (Reflector t _) = t
getTransform (NamedRotor r _) = getTransform r

getNotches:: Rotor e -> [e]
getNotches (Rotor _ n) = n
getNotches (Reflector _ n) = n
getNotches (NamedRotor r _) = getNotches r

getName:: Rotor e -> Maybe String
getName (Rotor _ n) = Nothing
getName (Reflector _ n) = Nothing
getName (NamedRotor _ n) = Just n

-----------------------------------------------
--             INSTANCES                     --
-----------------------------------------------
instance (Enum e, Ord e, Show e) => Show (Rotor e) where
    show (Rotor t ns) = "|" ++ show t ++ "| - "
                     ++ show (map (pad0 2 . fromEnum) ns)
    show (Reflector t ns) = "<" ++ show t ++ "> - "
                     ++ show (map (pad0 2 . fromEnum) ns)
    show (NamedRotor r n) = n ++ "-" ++ show r





instance Cipher Rotor where
    encrypt (Rotor t _)     = encrypt t
    encrypt (Reflector t _) = encrypt t
    encrypt (NamedRotor r _)     = encrypt r
    decrypt (Rotor t _)     = decrypt t
    decrypt (Reflector t _) = decrypt t
    decrypt (NamedRotor r _) = decrypt r
    letters (Rotor t _)     = letters t
    letters (Reflector t _) = letters t
    letters (NamedRotor r _) = letters r

    
----------------------------------------------
--          HELPER FUNCTIONS                --
----------------------------------------------  
pad0 :: Show p => Int -> p -> [Char]
pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x

displace :: (Num e) => Rotor e -> e -> Rotor e
displace (Rotor t is) j     = Rotor t $     map (\i -> i-j) is
displace (Reflector t is) j = Reflector t $ map (\i -> i-j) is
displace (NamedRotor r _) j = displace r j