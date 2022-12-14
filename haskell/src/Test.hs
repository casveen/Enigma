module Test (
    safeHead,
    findValue,
    noRepeats,
    isOnto,
    validRotor
) where
import Rotor(Rotor(..))
import Transform(Transform(..))
import Cipher ( Cipher(encrypt) )
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
-----------------------------------------------
--              TESTING                      --
----------------------------------------------- 
--check validity
--helper functions
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

findValue :: Eq a => [a] -> a -> Bool
findValue xs i = isJust (findIndex xs i)
findIndex :: Eq a => [a] -> a -> Maybe a
findIndex xs i = safeHead (dropWhile (/= i) xs)

noRepeats :: Eq a => [a] -> Bool
noRepeats [] = True
noRepeats (x:xs) = not (findValue xs x) && noRepeats xs


class Surjection f where
    isSurjective :: f -> Bool
    isOnto :: f -> Bool
    isOnto = isSurjective

instance (Ord e, Enum e) => Surjection (Transform e) where
    isSurjective tr =
        isPrefixOf (fmap toEnum [0..]) $ fmap (encrypt tr . toEnum) [0..]

validRotor :: (Ord e, Enum e) => Rotor e -> Bool
validRotor (Rotor t no) = isOnto t &&
                          noRepeats no &&
                          all (< n) no
    where
        n = length no
validRotor (Reflector t no) = validRotor (Rotor t no)