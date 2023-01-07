{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Cartridge (
    Cartridge(..),
    getNotchesFromCartridge,
    stepCartridge,
) where
import Rotor (Rotor(..), getNotches)
import Cipher (Cipher(..), TraceableCipher(..), logTrace, Cipherable)

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
                    eout x = decrypt r (x + p) - p
                    ein x =  encrypt r (x + p) - p
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
getNotchesFromRotors :: [Rotor e] -> [[e]]
getNotchesFromRotors = map getNotches

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