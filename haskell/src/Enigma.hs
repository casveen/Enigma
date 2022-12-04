module Enigma (
Enigma,
EnigmaState(..),
mkEnigma,
step,
enc,
encryptText,
tracedEncryptEnigma,
getPlugboard,
getCartridge,
getRotorPosition, 
setRotorPosition,
initialize
) where

--import Control.Monad.State
import Control.Monad.State.Strict
import Language
import Cipher (Cipher(..), PolyalphabeticCipher(..))
import Cartridge (Cartridge(..), stepCartridge, tracedEncryptCartridge)
import Plugboard (Plugboard(..))
import Rotor(Rotor(..))
import Control.Monad.Writer.Strict
import Debug.Trace
----------------------------------------------
--              ENIGMA                      --
----------------------------------------------    
type Enigma l o = State (EnigmaState l) o

data EnigmaState l = Enigma (Plugboard l) (Cartridge l) deriving (Eq)
{-instance HasLanguage EnigmaState where
    getLanguage (Enigma p c) = getLanguage c-}
instance (Show l) => Show (EnigmaState l) where
    show e@(Enigma p c) = "Enigma(" ++ ")\n" ++
                        "ID  POS                                 NOTCH\n" ++
                        "          ABCDEFGHIJKLMNOPQRSTUVWXYZ\n" ++
                        show p ++ "\n" ++
                        show c

----------------------------------------------
--           STATEFUL ACTIONS               --
----------------------------------------------
initialize :: EnigmaState l -> Enigma l ()
initialize = put

getPlugboard :: Enigma l (Plugboard l)
getPlugboard = state $ \e@(Enigma p _) -> (p, e)

getCartridge :: Enigma l (Cartridge l)
getCartridge = state $ \e@(Enigma _ c) -> (c, e)

step :: Enigma l ()
step = state $ \(Enigma plugging c) -> ((), Enigma plugging (stepCartridge c))

--instance PolyalphabeticCipher Enigma where
enc :: (Enum l, Ord l) => l -> Enigma l l
enc plaintext = do
    step
    rp <- getRotorPosition
    plugboard  <- getPlugboard
    cartridge  <- getCartridge
    return $
            --writer ()

            decrypt plugboard
            (encrypt cartridge
            (encrypt plugboard plaintext))

getRotorPosition :: Enigma l [Int]
getRotorPosition = state $ \e@(Enigma _ (Cartridge _ _ rotorPosition)) -> (reverse rotorPosition, e)



displace n (Rotor t is) j     = Rotor t $ map (\i -> mod (i-j) n) is
displace n (Reflector t is) j = Reflector t $ map (\i -> mod (i-j) n) is

--the enigma stores the positions, while the ringsetting affects when rotors turn
setRotorPosition :: (Enum e) => [e] -> [e] -> Enigma l ()
setRotorPosition ringSetting rotorPosition = do 
    c@(Cartridge rotors _ _) <- getCartridge

    let n = letters c
    let nrs = zipWith (\rs rp -> mod (fromEnum rp - fromEnum rs) n) ringSetting rotorPosition
    let displacedRotors = zipWith (displace n) rotors (map fromEnum (reverse ringSetting)) 
    state $ \(Enigma plugging (Cartridge _ rf _)) -> ((), Enigma plugging (Cartridge displacedRotors rf (reverse nrs)))



instance Cipher EnigmaState where
    encrypt (Enigma plugboard cartridge) plaintext =
            decrypt plugboard
            (encrypt cartridge
            (encrypt plugboard plaintext))
    decrypt = encrypt
    letters (Enigma _ cartridge) = letters cartridge

--instance Cipher (State (EnigmaState l)) where 


--tracedEncrypt :: (Language l, Enum o) => o -> MonadWriter (Enigma o l) a

tracedEncryptEnigma :: (Enum e, Ord e, Monoid (w e), Monad w) => e -> Enigma e (Writer (w e) e)
tracedEncryptEnigma plaintext = do --enigma context
    --step
    plugboard  <- getPlugboard
    cartridge  <- getCartridge
    return $ do --in the writer monad, wrapped in an enigma
        res <- writer (plaintext, return plaintext)
        res <- let encryption = encrypt plugboard res
            in writer (encryption, return encryption)
        res <- tracedEncryptCartridge cartridge res  
        let decryption = encrypt plugboard res 
        writer (decryption, return decryption)
        
        
        
        






{-instance PolyalphabeticMonoidCipher Enigma where 
    polyMonoidEncrypt cipherState = do 
        step 
        plugboard  <- getPlugboard
        cartridge  <- getCartridge
        return $ 
            monoidDecrypt plugboard <>
            monoidEncrypt cartridge <> 
            monoidEncrypt plugboard <>
            plaintext
    polyMonoidDecrypt = polyMonoidEncrypt-}

--encryptText :: (Traversable t) => Enigma (t o) l
encryptText (x:xs) = do
    c <-  enc x
    cs <- encryptText xs
    return (c:cs)
encryptText [] = do return []


mkEnigma :: (Enum e, Ord e) =>
    Plugboard e ->
    [Rotor e] ->
    Rotor e ->
    String ->
    String ->
    Maybe (EnigmaState e)
mkEnigma plugBoard rotors reflector rss rps = do
    rssRead <- safelyReadLetters rss
    rpsRead <- safelyReadLetters rps
    return $ let
        rssList = map fromEnum rssRead
        rpsList = map fromEnum rpsRead
        n       = letters plugBoard
        positions = reverse $ (\x y -> mod (x-y) n) <$> rpsList <*> rssList
        in
            Enigma plugBoard (Cartridge rotors reflector positions)

