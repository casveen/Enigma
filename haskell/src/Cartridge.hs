{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cartridge (
    Cartridge(..),
    getNotchesFromCartridge,
    stepCartridge,
) where
import Language (shiftLetter)
import Rotor (Rotor(..))
import Cipher (Cipher(..), TraceableCipher(..), logTrace)

----------------------------------------------
--              CARTRIDGE                   --
----------------------------------------------           
data Cartridge l = Cartridge [Rotor l] (Rotor l) [Int] deriving(Eq)

----------------------------------------------
--              INSTANCES                   --
----------------------------------------------     
instance (Show l) => Show (Cartridge l) where
    show (Cartridge rs rf ps) = foldr1 (++) (
                                map
                                (\(i,r,p) -> "R" ++ show i ++ ": [" ++ pad0 2 p ++ "] " ++ show r ++ "\n")
                                (zip3 [0..] rs ps)) ++
                                "Re:      " ++ show rf

instance Cipher Cartridge where
    encrypt c@(Cartridge rotors reflector positions) = total rotors positions
        where
            n = letters c
            total [] _  = encrypt reflector
            total _ []  = encrypt reflector
            total (r:rs) (p:ps) = eout . total rs ps . ein
                where
                    eout x = shiftLetter (decrypt r $ shiftLetter x p n) (-p) n
                    ein x = shiftLetter (encrypt r $ shiftLetter x p n) (-p) n
    decrypt = encrypt
    letters (Cartridge _ reflector _) = letters reflector

instance TraceableCipher Cartridge where 
    tracedEncrypt c@(Cartridge rotors reflector positions) m =
        let
            n = letters c
            total [] _ x = logTrace $ encrypt reflector x
            total rs [] x = total rs [0] x 
            total (r:rs) (p:ps) x = do
                res <- wein x
                res <- total rs ps res 
                weout res 
                where
                    ein y = shiftLetter (encrypt r $ shiftLetter y p n) (-p) n
                    eout y = shiftLetter (decrypt r $ shiftLetter y p n) (-p) n
                    wein  y = logTrace $ ein y
                    weout y = logTrace $ eout y
        in
            total rotors positions m

----------------------------------------------
--           HELPER FUNCTIONS               --
----------------------------------------------     
getNotchesFromRotors :: [Rotor e] -> [[Int]]
getNotchesFromRotors [] = []
getNotchesFromRotors ((Rotor _ notches):rs) = notches:getNotchesFromRotors rs
getNotchesFromRotors ((Reflector _ notches):rs) = notches:getNotchesFromRotors rs

getNotchesFromCartridge :: Cartridge e -> [[Int]]
getNotchesFromCartridge (Cartridge rs _ _) = getNotchesFromRotors rs

stepCartridge :: Cartridge l -> Cartridge l
stepCartridge c@(Cartridge rotors reflector positions) = Cartridge rotors reflector $ stepHelper positions (getNotchesFromRotors rotors) 1
    where
        n = letters c
        stepHelper [] _ _ = []
        stepHelper xs [] _ = xs
        stepHelper (p:ps) (ns:nss) carry =
            if elem p ns || carry==1
                then mod (p+1) n:stepHelper ps nss (if p `elem` ns then 1 else 0)
                else p:stepHelper ps nss 0

pad0 :: Show p => Int -> p -> [Char]
pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x

