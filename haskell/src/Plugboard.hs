module Plugboard (
    Plugboard(..),
    mkPlugboard
) where
import Transform ( Transform(..) )
import Cipher (Cipher(..))
import Data.Tuple ( swap )
import Data.Bimap ( fromList )
------------------------------------------------
--            PLUGBOARD                       --
------------------------------------------------  
newtype Plugboard e = Plugboard (Transform e) deriving(Eq)

-----------------------------------------------
--             INSTANCES                     --
-----------------------------------------------
instance (Show e) => Show (Plugboard e) where
    show (Plugboard t) = "P:        " ++ show t
instance Cipher Plugboard  where
    encrypt (Plugboard t) = encrypt t
    decrypt (Plugboard t) = decrypt t
    letters (Plugboard t) = letters t 

-----------------------------------------------
--           HELPER FUNCTIONS                --
-----------------------------------------------
mkPlugboard :: (Ord e) => [(e,e)] -> Plugboard e
mkPlugboard xs = 
    let 
        allPairs = xs ++ fmap swap xs
    in 
        Plugboard (Transform (fromList allPairs))