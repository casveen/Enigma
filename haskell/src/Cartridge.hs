{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cartridge (
    Cartridge(..),
    getNotchesFromCartridge,
    stepCartridge,
) where
import Rotor (Rotor(..))
import Cipher (Cipher(..), TraceableCipher(..), logTrace, Cipherable)
import Language(LetterOrdinal)
import Debug.Trace(trace)
import Data.Mod

----------------------------------------------
--              CARTRIDGE                   --
----------------------------------------------           
data Cartridge l = Cartridge [Rotor l] (Rotor l) [l] deriving(Eq)

----------------------------------------------
--              INSTANCES                   --
----------------------------------------------     


instance (Enum l, Ord l, Show l) => Show (Cartridge l) where
    show (Cartridge rs rf ps) = foldr1 (++) (
                                    map
                                        (\(i,r,p) -> "R" ++ show i ++ ": [" ++ pad0 2 ( fromEnum p) ++ "] " ++ show r ++ "\n")
                                        (zip3 [0..] rs ps)) ++
                                    "Re:      " ++ show rf

instance Cipher Cartridge where
    encrypt c@(Cartridge rotors reflector positions) = total rotors positions
        where
            total [] _  = encrypt reflector
            total _ []  = encrypt reflector
            total (r:rs) (p:ps) = eout . total rs ps . ein
                where
                    eout x = 
                        --trace ("decryption:" ++ (show . fromEnum $ x) ++ "+") --(show . map fromEnum $ positions) $
                            --("encryption:" ++ (show . fromEnum $ x) ++ "+" ++ 
                             --(show . unMod $ x) ++ ") + " ++ 
                             --(show . fromEnum $ p) ++ "=") $ -- ++
                             --(show . fromEnum $ (unMod p)) ++ ") = " ++  
                             --(show . fromEnum $ (x+p))) $ 
                            decrypt r (x + p) - p    --shiftLetter (decrypt r $ shiftLetter x p n) (-p) n
                    ein x =  
                        --trace
                             --("encryption:" ++ (show . fromEnum $ x) ++ "+") 
                             --(show . unMod $ x) ++ ") + " ++ 
                             --(show . fromEnum $ p) ++ "=" ) $ -- ++
                             --(show . fromEnum $ (unMod p)) ++ ") = " ++  
                             --(show . fromEnum $ (x+p))) $ 
                             encrypt r (x + p) - p    --shiftLetter (encrypt r $ shiftLetter x p n) (-p) n
    decrypt = encrypt
    letters (Cartridge _ reflector _) = letters reflector

instance TraceableCipher Cartridge where
    tracedEncrypt (Cartridge rotors reflector positions) m =
        let
            total [] _ x = logTrace $ encrypt reflector x
            total rs [] x = total rs [minBound] x
            total (r:rs) (p:ps) x = do
                res <- wein x
                res <- total rs ps res
                weout res
                where
                    ein y   = encrypt r (y + p) - p
                    eout y  = decrypt r (y + p) - p 
                    wein  y = logTrace $ ein y
                    weout y = logTrace $ eout y
        in
            total rotors positions m

----------------------------------------------
--           HELPER FUNCTIONS               --
----------------------------------------------     
--getNotchesFromRotors :: [Rotor e] -> [[Int]]
getNotchesFromRotors :: [Rotor e] -> [[e]]
getNotchesFromRotors [] = []
getNotchesFromRotors ((Rotor _ notches):rs) = notches:getNotchesFromRotors rs
getNotchesFromRotors ((Reflector _ notches):rs) = notches:getNotchesFromRotors rs

--getNotchesFromCartridge :: Cartridge e -> [[Int]]
getNotchesFromCartridge :: Cartridge e -> [[e]]
getNotchesFromCartridge (Cartridge rs _ _) = getNotchesFromRotors rs

stepCartridge :: (Cipherable l) => Cartridge l -> Cartridge l
stepCartridge (Cartridge rotors reflector positions) =
    Cartridge rotors reflector newPositions
    where
        notches           = getNotchesFromRotors rotors
        engagedBeforeTurn = zipWith elem positions notches
        newPositions      =
            zipWith3
            (\p e pe -> if e || pe then p+1 else p)
            positions
            engagedBeforeTurn
            (True : engagedBeforeTurn) --ensures first always turn, and holds engage of previous

pad0 :: Show p => Int -> p -> [Char]
pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x