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
initialize,
connect,
connectEnums
) where

import Control.Monad.State.Strict
    ( State, gets, modify, MonadState(put) )
import Cipher (Cipher(..), TraceableCipher(..), logTrace, Cipherable)
import Cartridge (Cartridge(..), stepCartridge)
import Plugboard (Plugboard(..))
import Rotor(Rotor(..), displace)
import Bombe.Wiring.Wiring ( Wiring(connectWire) )

reEnum :: (Enum c, Enum a) => a -> c
reEnum = toEnum . fromEnum

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

stepEnigma :: (Cipherable l) => EnigmaState l -> EnigmaState l
stepEnigma (Enigma plugging c) = Enigma plugging (stepCartridge c)

step :: (Cipherable l) => Enigma l ()
step = modify stepEnigma

getRotorPosition :: Enigma l [l]
getRotorPosition = gets $ reverse . (\(Cartridge _ _ rp) -> rp) . cartridge

--the enigma stores the positions, while the ringsetting affects when rotors turn
setRotorPosition :: (Enum e, Num e, Cipherable l) => [e] -> [e] -> Enigma l ()
setRotorPosition ringSetting rotorPosition = do
    (Cartridge rotors _ _) <- getCartridge
    let nrs = map reEnum $ zipWith (flip (-)) ringSetting rotorPosition
    let displacedRotors = zipWith
            displace
            rotors
            (map (toEnum . fromEnum) (reverse ringSetting))

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

instance (Enum l, Ord l, Show l) => Show (EnigmaState l) where
    show (Enigma p c) = "Enigma(" ++ ")\n" ++
                        "ID  POS                                 NOTCH\n" ++
                        "          ABCDEFGHIJKLMNOPQRSTUVWXYZ\n" ++
                        show p ++ "\n" ++
                        show c ++ "\n----------------------------------------------\n"

---------------------------------------------------
--                ENCRYPTIONS                    --
---------------------------------------------------
--                PARAMETRIC                     -- 
---------------------------------------------------
enc :: (Cipherable l) => l -> Enigma l l
enc plaintext = do
    step
    encryptWithoutStepping plaintext

encryptWithoutStepping :: (Cipherable l) => l -> Enigma l l
encryptWithoutStepping p = gets (`encrypt` p)

---------------------------------------------------
--                  MONADIC                      -- 
---------------------------------------------------
encryptMonad :: (Monad m, Cipherable e) => m e -> Enigma e (m e)
encryptMonad mp =
    do
        step
        gets $ \s -> do encrypt s <$> mp --with current state, encrypt each in given monad

encryptTraversableOfMonads :: (Traversable t, Cipherable e, Monad m) => t (m e) -> Enigma e (t (m e))
encryptTraversableOfMonads = traverse encryptMonad

---------------------------------------------------
--            SPECIFIC/ALIASES                   --
---------------------------------------------------
encryptText :: (Cipherable a) => [a] -> Enigma a [a]
encryptText = encryptTraversable

encryptTraversable :: (Traversable t, Cipherable e) => t e -> Enigma e (t e)
encryptTraversable = traverse enc

--------------------------------------------------
--                       HELPERS                --
--------------------------------------------------
mkEnigma ::
    (Cipherable e) =>
    Plugboard e ->
    [Rotor e] ->
    Rotor e ->
    [e] ->
    [e] ->
    Maybe (EnigmaState e)
mkEnigma plugBoard rotors reflector rss rps = do
    let rssRead = rss
    let rpsRead = rps
    return $ let
        rssList = rssRead
        rpsList = rpsRead
        positions = reverse $ (-) <$> rpsList <*> rssList
        in
            Enigma plugBoard (Cartridge rotors reflector positions)

----------------------------------------------------
--               WIRING                           --
---------------------------------------------------- 

connect :: (Cipherable e, Wiring w) => w -> e -> e -> Enigma e w
connect wiring cribLetter cipherLetter = do
    --in state monad 
    plugboard  <- getPlugboard
    let cipher = fromEnum cipherLetter
        crib   = fromEnum cribLetter
        n      = letters plugboard
    encryption <- encryptTraversableOfMonads [map toEnum [0..(n-1)]]
    return $
        foldl
            (\acc (p, c) ->
                connectWire acc (crib, p) (c, cipher)
            )
            wiring
            (zip [0..(n-1)] (map fromEnum (head encryption)))

connectEnums :: (Enum e, Cipherable f, Wiring w) => w -> e -> e -> Enigma f w
connectEnums wiring cribLetter cipherLetter = 
    connect wiring (reEnum cribLetter) (reEnum cipherLetter)