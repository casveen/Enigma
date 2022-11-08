module Enigma (
Enigma,
mkEnigma,
step,
encryptText
) where

import Control.Monad.State
import Language 
import Cipher (Cipher(..), PolyalphabeticCipher(..))
import Cartridge (Cartridge(..), stepCartridge)
import Plugboard (Plugboard(..))
import Rotor(Rotor(..))

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

--instance PolyalphabeticCipher Enigma where
enc :: (Language l, Enum o) => o -> Enigma o l
enc plaintext = do 
    step 
    plugboard  <- getPlugboard
    cartridge  <- getCartridge
    return $ 
            decrypt plugboard 
            (encrypt cartridge 
            (encrypt plugboard plaintext))
--encryptText :: (Traversable t) => Enigma (t o) l
encryptText (x:xs) = do
    c <-  enc x
    cs <- encryptText xs
    return (c:cs)
encryptText [] = do return []

mkEnigma :: (Language l) => 
    Plugboard l -> 
    [Rotor l] -> 
    Rotor l -> 
    String -> 
    String -> 
    Maybe (EnigmaState l)
mkEnigma plugBoard rotors reflector rss rps = do
    rssRead <- safelyReadLetters rss
    rpsRead <- safelyReadLetters rps
    return $ let 
        rssList = map fromEnum rssRead
        rpsList = map fromEnum rpsRead
        n       = letters . getLanguage $ plugBoard
        positions = reverse $ (\x y -> mod (x-y) n) <$> rpsList <*> rssList 
        in 
            Enigma plugBoard (Cartridge rotors reflector positions)
        

