module Transform (
    Transform(..),
    transformFromLanguage,
    transformFromOrdinals,
    idTransform
) where

import Language (LetterOrdinal)
import Cipher (Cipher(..))
--import Numeric.LinearAlgebra (Matrix, tr, toLists, fromList, toList, (#>), (><))
import Data.Bimap
import Data.List
import Debug.Trace(trace)
import Language


----------------------------------------------
--              TRANSFORM                   --
----------------------------------------------   
--data Transform l = Transform Permute l
newtype Transform l = Transform (Bimap l l) deriving (Eq)




instance (Enum l, Ord l) => Show (Transform l) where
    show t@(Transform m) =
        let
            allEncrypted = fmap ((m !>) . toEnum) [0 .. size m-1]
            listOfRhs = fmap (toEnum . fromEnum) allEncrypted :: [LetterOrdinal]
            showable mm = foldr1 (++) (fmap show mm)
        in
            showable listOfRhs
--for example 
--

--data Transform l = Transform (Matrix Double) l deriving(Eq)
--instance (Language l) => Show (Transform l) where
--    show p = show p

--instance HasLanguage Transform where
--    getLanguage (Transform _ language) = language

instance Cipher Transform where
    encrypt (Transform bi) = (!>) bi
    decrypt (Transform bi) = (!) bi
    letters (Transform bi) = length $ keys bi

transformFromLanguage :: (Enum a, Ord a) => [a] -> Transform a
transformFromLanguage ls = Transform (fromList $ zip ls (fmap toEnum [0..]))

--redundant?
transformFromOrdinals :: (Ord l, Enum l) => [LetterOrdinal] -> Transform l
transformFromOrdinals ls = Transform (fromList $ zip (fmap (toEnum . fromEnum) ls) (fmap toEnum [0..]))

idTransform :: (Ord e, Enum e, Bounded e) => Transform e
idTransform =
    let
        elms = [minBound ..]
    in
        Transform $ fromList (zip elms elms)
-----------------------------------------------------------------------