module Transform (
    Transform(..),
    transformFromLanguage
) where

import Language (Language(..), HasLanguage(..), Letter)
import Cipher (Cipher(..))
import Numeric.LinearAlgebra (Matrix, tr, toLists, fromList, toList, (#>), (><))

----------------------------------------------
--              TRANSFORM                   --
----------------------------------------------   
data Transform l = Transform (Matrix Double) l deriving(Eq)
instance (Language l) => Show (Transform l) where
    show (Transform m _) = foldr1 (++) (map (show . toLetter) (rowsToFirst m))
        where
            toLetter = toEnum :: Int -> Letter
            rowsToFirst mm = map firstNonZero (toLists mm)

instance HasLanguage Transform where
    getLanguage (Transform _ language) = language

instance Cipher Transform where
    --"just" a matrix mulitplication with corresponding row vector of letter
    encrypt (Transform m language) = encryptLetter 
        where
            encryptLetter l = toEnum (firstNonZero (toList $ m #> v l))
            v             l = fromList $ unitRow (fromEnum l) (letters language)
    decrypt (Transform m language) = decryptLetter
        where
            decryptLetter l = toEnum (firstNonZero (toList $ tr m #> v l))
            v             l = fromList $ unitRow (fromEnum l) (letters language)

transformFromLanguage ls l = Transform ((n><n) mat) l
    where
        n                           = letters l
        mat                         = elementsFromLanguage ls
        elementsFromLanguage []     = []
        elementsFromLanguage (l:ls) = unitRow (fromEnum l) n ++ elementsFromLanguage ls
-----------------------------------------------------------------------
--                          HELPER FUNCTIONS                         --
-----------------------------------------------------------------------
--MATRIX HELPERS
unitRow _ 0 = []
unitRow 0 n = 1:unitRow (-1) (n-1)
unitRow i n = 0:unitRow (i-1) (n-1)

firstNonZero []     = 0
firstNonZero (x:xs) = if x == 0 then 1 + firstNonZero xs else 0