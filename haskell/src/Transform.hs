module Transform (
    Transform(..),
    transformFromLanguage,
) where

--import Language (Letter)
import Cipher (Cipher(..))
--import Numeric.LinearAlgebra (Matrix, tr, toLists, fromList, toList, (#>), (><))
import Data.Bimap


----------------------------------------------
--              TRANSFORM                   --
----------------------------------------------   
--data Transform l = Transform Permute l
newtype Transform l = Transform (Bimap l l) deriving (Eq)

instance (Show l) => Show (Transform l) where
    show (Transform m) = show $ toList m --foldr1 (++) (fmap show (keys m))
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
-----------------------------------------------------------------------