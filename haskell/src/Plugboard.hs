module Plugboard (
    Plugboard(..),
    --tracedEncryptPlugboard,
    --tracedDecryptPlugboard,
    mkPlugboard
) where
import Transform
import Cipher (Cipher(..))
import Data.Tuple ( swap )
import Control.Monad.Writer.Strict ( Writer, MonadWriter(writer), runWriter )
import Data.Bimap ( fromList )
import Debug.Trace
------------------------------------------------
--            PLUGBOARD                       --
------------------------------------------------  
newtype Plugboard e = Plugboard (Transform e) deriving(Eq)
instance (Show e) => Show (Plugboard e) where
    show (Plugboard t) = "P:        " ++ show t
instance Cipher Plugboard  where
    encrypt (Plugboard t) = encrypt t
    decrypt (Plugboard t) = decrypt t
    letters (Plugboard t) = letters t 

{-
tracedEncryptPlugboard :: (Ord e, Enum e, Monoid (w e), Monad w) => Plugboard e -> e -> Writer (w e) e
tracedEncryptPlugboard plugboard m = 
    let c = encrypt plugboard m 
    in  

tracedDecryptPlugboard :: (Ord e, Enum e, Monoid (w e), Monad w) => Plugboard e -> e -> Writer (w e) e
tracedDecryptPlugboard plugboard m = return $ decrypt plugboard (fst $ runWriter m)
-}

mkPlugboard :: (Ord e, Show e) => [(e,e)] -> Plugboard e
mkPlugboard xs = 
    let 
        allPairs = xs ++ fmap swap xs
    in 
        Plugboard (Transform (fromList allPairs))