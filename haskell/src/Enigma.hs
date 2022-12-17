{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Enigma (
Enigma,
EnigmaState(..),
mkEnigma,
step,
stepEnigma,
enc,
encryptText,
encryptTraversable,
encryptTraversableOfMonads,
getPlugboard,
getCartridge,
getRotorPosition,
setRotorPosition,
initialize
) where

--import Control.Monad.State
import Control.Monad.State.Strict
import Language ( safelyReadLetters )
import Cipher (Cipher(..), TraceableCipher(..), logTrace)
import Cartridge (Cartridge(..), stepCartridge)
import Plugboard (Plugboard(..))
import Rotor(Rotor(..))
--import Control.Monad.Writer.Strict
----------------------------------------------
--              ENIGMA                      --
----------------------------------------------    
type Enigma l o = State (EnigmaState l) o

data EnigmaState l = Enigma (Plugboard l) (Cartridge l) deriving (Eq)
{-instance HasLanguage EnigmaState where
    getLanguage (Enigma p c) = getLanguage c-}
instance (Show l) => Show (EnigmaState l) where
    show (Enigma p c) = "Enigma(" ++ ")\n" ++
                        "ID  POS                                 NOTCH\n" ++
                        "          ABCDEFGHIJKLMNOPQRSTUVWXYZ\n" ++
                        show p ++ "\n" ++
                        show c

--composeTransforms =
--getTransformFromEnigma (Enigma p c) = composeTransforms (getTransformFromCartridge c) p

----------------------------------------------
--           STATEFUL ACTIONS               --
----------------------------------------------
initialize :: EnigmaState l -> Enigma l ()
initialize = put

getPlugboard :: Enigma l (Plugboard l)
getPlugboard = state $ \e@(Enigma p _) -> (p, e)

getCartridge :: Enigma l (Cartridge l)
getCartridge = state $ \e@(Enigma _ c) -> (c, e)

--getTransform :: Enigma l (Transform l)
--getTransform = state $ \e -> (getTransformFromEnigma e, e)

stepEnigma :: EnigmaState e -> EnigmaState e
stepEnigma (Enigma plugging c) = Enigma plugging (stepCartridge c)

step :: Enigma l ()
step = state $ \e -> ((), stepEnigma e)

--instance PolyalphabeticCipher Enigma where
enc :: (Enum l, Ord l) => l -> Enigma l l
enc plaintext = do
    step
    encryptWithoutStepping plaintext

encryptWithoutStepping :: (Enum l, Ord l) => l -> Enigma l l
encryptWithoutStepping plaintext = do
    plugboard  <- getPlugboard
    cartridge  <- getCartridge
    return $
            decrypt plugboard
            (encrypt cartridge
            (encrypt plugboard plaintext))


getRotorPosition :: Enigma l [Int]
getRotorPosition = state $ \e@(Enigma _ (Cartridge _ _ rotorPosition)) -> (reverse rotorPosition, e)



displace :: Int -> Rotor e -> Int -> Rotor e
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
----------------------------------------------
--                   Instances              --
----------------------------------------------
instance Cipher EnigmaState where
    encrypt (Enigma plugboard cartridge) plaintext =
            decrypt plugboard
            (encrypt cartridge
            (encrypt plugboard plaintext))
    decrypt = encrypt
    letters (Enigma _ cartridge) = letters cartridge

instance TraceableCipher EnigmaState where
    tracedEncrypt (Enigma plugboard cartridge) plaintext =
        do --in the writer monad
            res <- logTrace   plaintext
            res <- logTrace $ encrypt plugboard res
            res <- tracedEncrypt cartridge res
            logTrace $ encrypt plugboard res


{-
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
        
        -}








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
encryptText :: (Enum a, Ord a) => [a] -> Enigma a [a]
encryptText = encryptTraversable

encryptTraversable :: (Traversable t, Enum e, Ord e) => t e -> Enigma e (t e)
encryptTraversable = traverse enc

encryptMonad :: (Monad m, Enum e, Ord e) => m e -> Enigma e (m e)
encryptMonad mp =
    --we are in the enigma monad
    do
        currentState <- get --current state
        --in the plaintextmonad
        let cs = do {p <- mp ; return $ evalState (encryptWithoutStepping p) currentState}
        -- mc is m (Enigma e), we want Enigma e (m e) or (m e)


        state $ const (cs, stepEnigma currentState)



encryptTraversableOfMonads :: (Traversable t, Enum e, Ord e, Monad m) => t (m e) -> Enigma e (t (m e))
encryptTraversableOfMonads = traverse encryptMonad


        --we are in the enigma monad!
        --let nextState = stepEnigma 
        --    mEncrypt  =  -- encryption "without" state

        --do  
        --    p <- mp
        --    let c = enc p -- c :: m (Enigma l l), in list, we have undet answer and state. 
            --collapse state.
            --the return type of enc being the state itself is a big problem!
            --something representing a deterministic state should not have state type as returntype. 
            --since these will be wrapped in the monad





mkEnigma ::
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


