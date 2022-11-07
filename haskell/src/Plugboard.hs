module Plugboard (
Plugboard(..)
) where
import Transform (Transform)
import Language (Language, HasLanguage)
import Cipher (Cipher)
------------------------------------------------
--            PLUGBOARD                       --
------------------------------------------------  
newtype Plugboard l = Plugboard (Transform l) deriving(Eq)
instance HasLanguage Plugboard where
    getLanguage (Plugboard t) = getLanguage t
instance (Language l) => Show (Plugboard l) where
    show (Plugboard t) = "P:        " ++ show t
instance Cipher Plugboard  where
    encrypt (Plugboard t) = encrypt t
    decrypt (Plugboard t) = decrypt t