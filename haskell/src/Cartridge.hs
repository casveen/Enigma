{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Cartridge (
    Cartridge(..),
    getNotchesFromCartridge,
    stepCartridge,
    tracedEncryptCartridge
) where
import Language (shiftLetter)
import Rotor (Rotor(..))
import Cipher (Cipher(..))
import Control.Monad.Writer.Strict
import Debug.Trace
----------------------------------------------
--              CARTRIDGE                   --
----------------------------------------------           
data Cartridge l = Cartridge [Rotor l] (Rotor l) [Int] deriving(Eq)
--instance HasLanguage Cartridge where
--   getLanguage (Cartridge _ r _) = getLanguage r
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
            --total :: (Cipher s) => [s] -> (Letter -> Letter)
            total [] _  = encrypt reflector
            --total [] __ = monadicEncrypt reflector ---XXX WRONG!
            total _ []  = encrypt reflector ---XXX WRONG!
            total (r:rs) (p:ps) = eout . total rs ps . ein
                where
                    eout x = shiftLetter (decrypt r $ shiftLetter x (p) n) (-p) n
                    ein x = shiftLetter (encrypt r $ shiftLetter x (p) n) (-p) n
            --might step wrong way!
    decrypt = encrypt
    letters (Cartridge _ reflector _) = letters reflector

getNotchesFromRotors [] = []
getNotchesFromRotors ((Rotor _ notches):rs) = notches:getNotchesFromRotors rs
getNotchesFromRotors ((Reflector _ notches):rs) = notches:getNotchesFromRotors rs

getNotchesFromCartridge (Cartridge rs r _) = getNotchesFromRotors rs

stepCartridge :: Cartridge l -> Cartridge l
stepCartridge c@(Cartridge rotors reflector positions) = Cartridge rotors reflector $ stepHelper positions (getNotchesFromRotors rotors) 1
    where
        n = letters c
        --increase position if p is in ps, or if previous was in ps
        --stepHelper (p:ps) [] _ = p:(stepHelper ps [] 0) --unsure...
        --turn if positions + ringsetting = notching
        stepHelper [] _ _ = []
        stepHelper (p:ps) (ns:nss) carry =
            if elem p ns || carry==1
                then mod (p+1) n:stepHelper ps nss (if p `elem` ns then 1 else 0)
                else p:stepHelper ps nss 0

pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x

tracedEncryptCartridge :: (Enum e, Ord e, Monoid (w e), Monad w) => Cartridge e -> e -> Writer (w e) e
tracedEncryptCartridge c@(Cartridge rotors reflector positions) m =
    let
        n = letters c
        --total :: (Enum e, Ord e, Monoid w) => [Rotor e] -> [Int] -> Writer w e -> Writer w e
        total [] _ x = writer (encrypt reflector x, return $ encrypt reflector x)
        total (r:rs) (p:ps) x = do
            res <- wein x
            res <- total rs ps res 
            weout res 
            where
                ein y = shiftLetter (encrypt r $ shiftLetter y p n) (-p) n
                eout y = shiftLetter (decrypt r $ shiftLetter y p n) (-p) n

                --wein :: (Enum e, Ord e, Monoid w) => Writer w e -> Writer w e
                wein  y =  writer (ein y, return $ ein y)
                --weout :: (Enum e, Ord e, Monoid w) => Writer w e -> Writer w e
                weout y = writer (eout y, return $ eout y)
    in
        total rotors positions m