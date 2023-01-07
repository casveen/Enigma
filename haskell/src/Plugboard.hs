module Plugboard (
    Plugboard(..),
    mkPlugboard
) where
import Transform ( Transform(..) )
import Cipher (Cipher(..))
import Data.Tuple ( swap )
import Data.Bimap ( fromList )
import Data.Bifunctor (Bifunctor(bimap))
------------------------------------------------
--            PLUGBOARD                       --
------------------------------------------------  
newtype Plugboard e = Plugboard (Transform e) deriving(Eq)

-----------------------------------------------
--             INSTANCES                     --
-----------------------------------------------
instance (Enum e, Ord e) => Show (Plugboard e) where
    show (Plugboard t) = "P:        " ++ show t
instance Cipher Plugboard  where
    encrypt (Plugboard t) = encrypt t
    decrypt (Plugboard t) = decrypt t
    letters (Plugboard t) = letters t

-----------------------------------------------
--           HELPER FUNCTIONS                --
-----------------------------------------------
mkPlugboard :: (Enum e, Ord f, Enum f) => [(e, e)] -> Plugboard f
mkPlugboard xs =
    let
        reEnum :: (Enum e, Enum f) => [(e,e)] -> [(f,f)]
        reEnum = map (bimap (toEnum . fromEnum) (toEnum . fromEnum) )
        allPairs = xs ++ fmap swap xs
    in
        Plugboard
            (Transform
                (fromList
                    $ reEnum allPairs))