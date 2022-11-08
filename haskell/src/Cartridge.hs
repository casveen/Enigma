module Cartridge (
    Cartridge(..),
    getNotchesFromCartridge,
    stepCartridge
) where 
import Language (Language(..), HasLanguage(..), shiftLetter)
import Rotor (Rotor(..))
import Cipher (Cipher(..))

----------------------------------------------
--              CARTRIDGE                   --
----------------------------------------------           
data Cartridge l = Cartridge [Rotor l] (Rotor l) [Int] deriving(Eq)
instance HasLanguage Cartridge where
    getLanguage (Cartridge _ r _) = getLanguage r
instance (Language l) => Show (Cartridge l) where
    show (Cartridge rs rf ps) = foldr1 (++) (
                                map
                                (\(i,r,p) -> "R" ++ show i ++ ": [" ++ pad0 2 p ++ "] " ++ show r ++ "\n")
                                (zip3 [0..] rs ps)) ++
                                "Re:      " ++ show rf
instance Cipher Cartridge where
    encrypt c@(Cartridge rotors reflector positions) = total rotors positions
        where
            n = letters . getLanguage $ c
            --total :: (Cipher s) => [s] -> (Letter -> Letter)
            total [] _  = encrypt reflector
            --total [] __ = monadicEncrypt reflector ---XXX WRONG!
            total _ []  = encrypt reflector ---XXX WRONG!
            total (r:rs) (p:ps) = eout . total rs ps . ein
                where
                    ein  = \x -> shiftLetter (encrypt r $ shiftLetter x p n) (-p) n
                    eout = \x -> shiftLetter (decrypt r $ shiftLetter x p n) (-p) n
            --might step wrong way!
    decrypt = encrypt

getNotchesFromRotors [] = []
getNotchesFromRotors ((Rotor _ notches):rs) = notches:getNotchesFromRotors rs
getNotchesFromRotors ((Reflector _ notches):rs) = notches:getNotchesFromRotors rs

getNotchesFromCartridge (Cartridge rs r _) = getNotchesFromRotors rs

stepCartridge c@(Cartridge rotors reflector positions) = Cartridge rotors reflector $ stepHelper positions (getNotchesFromRotors rotors) 1
    where
        n = letters . getLanguage $ c
        --increase position if p is in ps, or if previous was in ps
        stepHelper [] [] _ = []
        stepHelper (p:ps) (ns:nss) carry =
            if elem p ns || carry==1
                then mod (p+1) n:stepHelper ps nss (if p `elem` ns then 1 else 0)
                else p:stepHelper ps nss 0

pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x