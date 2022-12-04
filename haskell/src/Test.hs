module Test (
    safeHead,
    findValue, 
    noRepeats,
    isOnto,
    validRotor
) where
import Rotor(Rotor(..))
import Transform(Transform(..))
import Cipher
import Numeric.LinearAlgebra (det, tr)
import Data.Maybe ( fromMaybe, isJust, isNothing )
import Test.HUnit
import Data.List (isPrefixOf)
-----------------------------------------------
--              TESTING                      --
----------------------------------------------- 
--check validity
--helper functions
safeHead [] = Nothing
safeHead (x:xs) = Just x

--find :: (Eq a) => a -> [a] -> Maybe a
findValue xs i = isJust (findIndex xs i)
findIndex xs i = safeHead (dropWhile (/= i) xs)

noRepeats [] = True
noRepeats (x:xs) = not (findValue xs x) && noRepeats xs


class Surjection f where 
    isSurjective :: f -> Bool
    isOnto :: f -> Bool 
    isOnto = isSurjective

instance (Ord e, Enum e) => Surjection (Transform e) where 
    isSurjective tr@(Transform bi) = 
        isPrefixOf (fmap toEnum [0..]) $ (fmap (encrypt tr) (fmap toEnum [0..]))

--validRotor :: Rotor -> Bool
validRotor (Rotor t no) = isOnto t &&
                          noRepeats no &&
                          all (< n) no
    where
        n = length no
validRotor (Reflector t no) = validRotor (Rotor t no)
--                                isSymmetricTransformation t