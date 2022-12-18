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

import Control.Monad.State.Strict
    ( State, gets, modify, MonadState(put) )
import Language ( safelyReadLetters )
import Cipher (Cipher(..), TraceableCipher(..), logTrace)
import Cartridge (Cartridge(..), stepCartridge)
import Plugboard (Plugboard(..))
import Rotor(Rotor(..))

----------------------------------------------
--              ENIGMA                      --
----------------------------------------------    
type Enigma l o = State (EnigmaState l) o

data EnigmaState l = Enigma {plugboard :: Plugboard l, cartridge :: Cartridge l} deriving (Eq)

----------------------------------------------
--           STATEFUL ACTIONS               --
----------------------------------------------
initialize :: EnigmaState l -> Enigma l ()
initialize = put

getPlugboard :: Enigma l (Plugboard l)
getPlugboard = gets plugboard

getCartridge :: Enigma l (Cartridge l)
getCartridge = gets cartridge

stepEnigma :: EnigmaState e -> EnigmaState e
stepEnigma (Enigma plugging c) = Enigma plugging (stepCartridge c)

step :: Enigma l ()
step = modify stepEnigma

getRotorPosition :: Enigma l [Int]
getRotorPosition = gets $ reverse . (\(Cartridge _ _ rp) -> rp) . cartridge

displace :: Int -> Rotor e -> Int -> Rotor e
displace n (Rotor t is) j     = Rotor t $     map (\i -> mod (i-j) n) is
displace n (Reflector t is) j = Reflector t $ map (\i -> mod (i-j) n) is

--the enigma stores the positions, while the ringsetting affects when rotors turn
setRotorPosition :: (Enum e) => [e] -> [e] -> Enigma l ()
setRotorPosition ringSetting rotorPosition = do
    c@(Cartridge rotors _ _) <- getCartridge
    let n = letters c
    let nrs = zipWith (\rs rp -> mod (fromEnum rp - fromEnum rs) n) ringSetting rotorPosition
    let displacedRotors = zipWith (displace n) rotors (map fromEnum (reverse ringSetting))
    modify $ \(Enigma plugging (Cartridge _ rf _)) -> Enigma plugging (Cartridge displacedRotors rf (reverse nrs))

----------------------------------------------
--                   Instances              --
----------------------------------------------
instance Cipher EnigmaState where
    encrypt (Enigma plugboard cartridge) =
            decrypt plugboard .
            encrypt cartridge .
            encrypt plugboard
    decrypt = encrypt
    letters (Enigma _ cartridge) = letters cartridge

instance TraceableCipher EnigmaState where
    tracedEncrypt (Enigma plugboard cartridge) plaintext =
        do --in the writer monad
            res <- logTrace   plaintext
            res <- logTrace $ encrypt plugboard res
            res <- tracedEncrypt cartridge res
            logTrace $ encrypt plugboard res

instance (Show l) => Show (EnigmaState l) where
    show (Enigma p c) = "Enigma(" ++ ")\n" ++
                        "ID  POS                                 NOTCH\n" ++
                        "          ABCDEFGHIJKLMNOPQRSTUVWXYZ\n" ++
                        show p ++ "\n" ++
                        show c

---------------------------------------------------
--                ENCRYPTIONS                    --
---------------------------------------------------
--                PARAMETRIC                     -- 
---------------------------------------------------
enc :: (Enum l, Ord l) => l -> Enigma l l
enc plaintext = do
    step
    encryptWithoutStepping plaintext

encryptWithoutStepping :: (Enum l, Ord l) => l -> Enigma l l
encryptWithoutStepping p = gets (`encrypt` p)

---------------------------------------------------
--                  MONADIC                      -- 
---------------------------------------------------
encryptMonad :: (Monad m, Enum e, Ord e) => m e -> Enigma e (m e)
encryptMonad mp =
    do
        step
        gets $ \s -> do encrypt s <$> mp --with current state, encrypt each in given monad

encryptTraversableOfMonads :: (Traversable t, Enum e, Ord e, Monad m) => t (m e) -> Enigma e (t (m e))
encryptTraversableOfMonads = traverse encryptMonad

---------------------------------------------------
--            SPECIFIC/ALIASES                   --
---------------------------------------------------
encryptText :: (Enum a, Ord a) => [a] -> Enigma a [a]
encryptText = encryptTraversable

encryptTraversable :: (Traversable t, Enum e, Ord e) => t e -> Enigma e (t e)
encryptTraversable = traverse enc

--------------------------------------------------
--                       HELPERS                --
--------------------------------------------------
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