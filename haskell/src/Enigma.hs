module Enigma (
Enigma,
Plugboard(..),
Cartridge(..),
Rotor(..),
English(..),
Minimal(..),
Letter(..),
Language(..),
mkEnigma,
step,
transformFromLanguage,
readLetters,
monadicEncrypt,
encrypt,
encryptText,
monadicPolyEncrypt
) where

import Data.Maybe ( fromMaybe, isJust, isNothing )
import Control.Monad.State
import Language
import Cipher (PolyalphabeticCipher)
import Cartridge (Cartridge)

----------------------------------------------
--              ENIGMA                      --
----------------------------------------------    
type Enigma o l = State (EnigmaState l) o

data EnigmaState l = Enigma (Plugboard l) (Cartridge l) deriving (Eq)
instance HasLanguage EnigmaState where
    getLanguage (Enigma p c) = getLanguage c
instance (Language l) => Show (EnigmaState l) where
    show e@(Enigma p c) = "Enigma(" ++ show (getLanguage e) ++ ")\n" ++
                        "ID  POS                                 NOTCH\n" ++
                        "          ABCDEFGHIJKLMNOPQRSTUVWXYZ\n" ++
                        show p ++ "\n" ++
                        show c

----------------------------------------------
--           STATEFUL ACTIONS               --
----------------------------------------------
getPlugboard :: Enigma (Plugboard l) l 
getPlugboard = state $ \e@(Enigma p _) -> (p, e)

getCartridge :: Enigma (Cartridge l) l 
getCartridge = state $ \e@(Enigma _ c) -> (c, e)

step :: (Language l) => Enigma () l
step = state $ \(Enigma plugging c) -> ((), Enigma plugging (stepCartridge c))

instance PolyalphabeticCipher Enigma where
    polyEncrypt :: (Language l) => o -> Enigma o l
    polyEncrypt plaintext = do 
        step 
        plugboard  <- getPlugboard
        cartridge  <- getCartridge
        return $ 
                decrypt plugboard 
                (encrypt cartridge 
                (encrypt plugboard (Identity plaintext)))

encryptText = mapM polyEncrypt
        
mkEnigma plugBoard rotors reflector rss rps = Enigma plugBoard (Cartridge rotors reflector positions)
    where
        n = letters . getLanguage $ plugBoard
        rssList   = map fromEnum $ readLetters rss
        rpsList   = map fromEnum $ readLetters rps
        positions = reverse $ (\x y -> mod (x-y) n) <$> rpsList <*> rssList