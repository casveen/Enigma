module Enigma (
Enigma,
Plugboard,
Cartridge,
Rotor,
English,
Minimal,
Letter(..),
Language,
mkEnigma,
ic, iic, iiic, ukw, im, iim, iiim, ukwm,
e, em, m3Navy,
viii,
vi,
v,
beta,
thinReflectorC,
idTransform,
step,
mplug,
transformFromLanguage,
readLetters,
donitzPlugboard,
monadicEncrypt,
encrypt,
encryptText,
monadicPolyEncrypt
) where

import Data.Maybe ( fromMaybe, isJust, isNothing )
import Numeric.LinearAlgebra (Matrix, tr, det, (><), toLists, fromList, toList, (#>))
import Data.Functor.Identity
--import Control.Monad.Writer

--data Pair a b  = Pa a b 

--data SequenceReader a = Seq a String (a->String)

--recordBefore (Seq a str) r = Seq a (r ++ str)
--instance Functor SequenceReader where
    --   a->b     f a
--    fmap f (Seq v strs c) = Seq (f v) (strs ++ c v) c      
    --mempty :: m  
    --mappend :: m -> m -> m  
    --mappend (Seq l ls) (Seq r rs) = Seq (l++r)
    --mconcat :: [m] -> m  
    --mempty = Seq []
    --fmap :: (a -> b) -> f a -> f b
    --fmap :: (a->b) -> SequenceReader a -> SequenceReader b
    --fmap f (Seq (Pa v r)) = Seq (Pa (f v) (Pa v r))

--instance Monad SequenceReader where
--    --(>>=) :: Sequence -> (a -> Sequence a) -> Sequence a
--    (>>=) (Seq v str) f = f v --recordBefore (f v) (str ++ " -> ")
--
--    --return v = Seq v (show v)







-----------------------------------------------------------------------
--                          HELPER FUNCTIONS                         --
-----------------------------------------------------------------------
pad0 n x = take ( n - length sx) (cycle "0") ++ sx
    where sx = show x

fpow 0 f = id
fpow n f = f . fpow (n-1) f

--MATRIX HELPERS
unitRow _ 0 = []
unitRow 0 n = 1:unitRow (-1) (n-1)
unitRow i n = 0:unitRow (i-1) (n-1)

firstNonZero []     = 0
firstNonZero (x:xs) = if x == 0 then 1 + firstNonZero xs else 0

-----------------------------------------------------------------------
--                          LANGUAGE                                 --
-----------------------------------------------------------------------
class (Show a) => Language a where
    letters :: a -> Int
class HasLanguage a where
    getLanguage :: (Language l) => a l -> l

--technically there should be a class for cipher (rotors, ppplugboard etc) which do not
--change the cipher upon encryption
--and another for stepping ciphers(the enigma itself), which change from encryption to encryption.
class Cipher c where
    --monadic(de/en)encrypt de(encrypts a monad with enums and returns the en/de encrypted enums in the given monad
    monadicEncrypt :: (Language l, Monad m, Enum e) => c l -> m e -> m e
    monadicDecrypt :: (Language l, Monad m, Enum e) => c l -> m e -> m e

class PolyalphabeticCipher c where
    --monadic(de/en)encrypt de(encrypts a monad with enums and returns the en/de encrypted enums in the given monad
    --a polyalphabetic cipher changes from encryptionto encryption, so it has to return the "new" cipher for each encryption
    monadicPolyEncrypt :: (Language l, Monad m, Enum e) => m e -> c l -> (m e, c l)
    monadicPolyDecrypt :: (Language l, Monad m, Enum e) => m e -> c l -> (m e, c l)

--encrypt cipher plaintext = runIdentity $ monadicEncrypt cipher (Identity plaintext)











data English = English deriving(Eq, Show)
data Minimal = Minimal deriving(Eq, Show)
instance Language English where
    letters _ = 26
instance Language Minimal where
    letters _ = 4

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Eq,Enum,Show,Read)

--instance Monad (Writer Letter) where  
--    return x = Writer (x, show x)  
    --(Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

readLetters :: String -> [Letter]
readLetters = map (\ l -> read [l])

shiftLetter :: (Enum a) => a -> Int -> Int -> a
shiftLetter l p n = toEnum $ mod (fromEnum l + p) n

monadicShiftLetter :: (Monad m, Enum a) => m a -> Int -> Int -> m a
monadicShiftLetter l p n = (\x -> toEnum $ mod (fromEnum x + p) n) <$> l
----------------------------------------------
--              TRANSFORM                   --
----------------------------------------------   
data Transform l = Transform (Matrix Double) l deriving(Eq)
instance (Language l) => Show (Transform l) where
    show (Transform m l) = foldr1 (++) (map (show . toLetter) (rowsToFirst m))
        where
            toLetter = toEnum :: Int -> Letter
            --toChar = show toEnum 
            firstNonZero []     = 0
            firstNonZero (x:xs) = if x/=0 then 0 else 1 + firstNonZero xs
            rowsToFirst mm = map firstNonZero (toLists mm)
instance HasLanguage Transform where
    getLanguage (Transform _ language) = language
instance Cipher Transform where
    --"just" a matrix mulitplication with corresponding row vector of letter
    monadicEncrypt (Transform m language) letter = letter >>= \x -> return $ encryptLetter x
        where
            encryptLetter l = toEnum (firstNonZero (toList $ m #> v l))
            v             l = fromList $ unitRow (fromEnum l) (letters language)
    monadicDecrypt (Transform m language) letter = 
        letter >>= \x -> return $ decryptLetter x
        where
            decryptLetter l = toEnum (firstNonZero (toList $ tr m #> v l))
            v             l = fromList $ unitRow (fromEnum l) (letters language)



----------------------------------------------
--              ROTOR                       --
----------------------------------------------    
data Rotor l = Rotor (Transform l) [Int] | Reflector (Transform l) [Int] deriving(Eq)
instance (Language l) => Show (Rotor l) where
    show (Rotor t ns) = "|" ++ show t ++ "| - "
                     ++ show (map (pad0 2) ns)
    show (Reflector t ns) = "<" ++ show t ++ "> - "
                     ++ show (map (pad0 2) ns)
instance HasLanguage Rotor where
    getLanguage (Rotor t _) = getLanguage t
    getLanguage (Reflector t _) = getLanguage t
instance Cipher Rotor where
    monadicEncrypt (Rotor t _)     = monadicEncrypt t
    monadicEncrypt (Reflector t _) = monadicEncrypt t
    monadicDecrypt (Rotor t _)     = monadicDecrypt t
    monadicDecrypt (Reflector t _) = monadicDecrypt t


transformFromLanguage ls l = Transform ((n><n) mat) l
    where
        n                           = letters l
        mat                         = elementsFromLanguage ls
        elementsFromLanguage []     = []
        elementsFromLanguage (l:ls) = unitRow (fromEnum l) n ++ elementsFromLanguage ls



------------------------------------------------
--            PLUGBOARD                       --
------------------------------------------------  
newtype Plugboard l = Plugboard (Transform l) deriving(Eq)
instance HasLanguage Plugboard where
    getLanguage (Plugboard t) = getLanguage t
instance (Language l) => Show (Plugboard l) where
    show (Plugboard t) = "P:        " ++ show t
instance Cipher Plugboard  where
    monadicEncrypt (Plugboard t) = monadicEncrypt t
    monadicDecrypt (Plugboard t) = monadicDecrypt t


-----------------------------------------------------------------------
--                          ENIGMA WHEELS                            --
-----------------------------------------------------------------------
idTransform =     transformFromLanguage (readLetters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") English
ic   = Rotor     (transformFromLanguage (readLetters "DMTWSILRUYQNKFEJCAZBPGXOHV") English) [0]
iic  = Rotor     (transformFromLanguage (readLetters "HQZGPJTMOBLNCIFDYAWVEUSRKX") English) [0]
iiic = Rotor     (transformFromLanguage (readLetters "UQNTLSZFMREHDPXKIBVYGJCWOA") English) [0]
ukw  = Reflector (transformFromLanguage (readLetters "QYHOGNECVPUZTFDJAXWMKISRBL") English) [0]

i    = Rotor     (transformFromLanguage (readLetters "EKMFLGDQVZNTOWYHXUSPAIBRCJ") English) [fromEnum Q]
ii   = Rotor     (transformFromLanguage (readLetters "AJDKSIRUXBLHWTMCQGZNPYFVOE") English) [fromEnum E]
iii  = Rotor     (transformFromLanguage (readLetters "BDFHJLCPRTXVZNYEIWGAKMUSQO") English) [fromEnum V]
iv   = Rotor     (transformFromLanguage (readLetters "ESOVPZJAYQUIRHXLNFTGKDCMWB") English) [fromEnum J]
v    = Rotor     (transformFromLanguage (readLetters "VZBRGITYUPSDNHLXAWMJQOFECK") English) [fromEnum Z]
vi   = Rotor     (transformFromLanguage (readLetters "JPGVOUMFYQBENHZRDKASXLICTW") English) [fromEnum Z, fromEnum M]
vii  = Rotor     (transformFromLanguage (readLetters "NZJHGRCXMYSWBOUFAIVLPEKQDT") English) [fromEnum Z, fromEnum M]
viii = Rotor     (transformFromLanguage (readLetters "FKQHTLXOCBJSPDZRAMEWNIUYGV") English) [fromEnum Z, fromEnum M]
beta  = Rotor (transformFromLanguage (readLetters "LEYJVCNIXWPBQMDRTAKZGFUHOS") English) []
gamma = Rotor (transformFromLanguage (readLetters "FSOKANUERHMBTIYCWLQPZXVGJD") English) []


reflectorA = Reflector (transformFromLanguage (readLetters "EJMZALYXVBWFCRQUONTSPIKHGD") English) []
reflectorB = Reflector (transformFromLanguage (readLetters "YRUHQSLDPXNGOKMIEBFZCWVJAT") English) []
reflectorC = Reflector (transformFromLanguage (readLetters "FVPJIAOYEDRZXWGCTKUQSBNMHL") English) []
thinReflectorB = Reflector (transformFromLanguage (readLetters "ENKQAUYWJICOPBLMDXZVFTHRGS") English) []
thinReflectorC = Reflector (transformFromLanguage (readLetters "RDOBJNTKVEHMLFCWZAXGYIPSUQ") English) []
mplug =            transformFromLanguage [A, B, C, D] Minimal
im    = Rotor     (transformFromLanguage [B, C, D, A] Minimal) [0]
iim   = Rotor     (transformFromLanguage [C, D, A, B] Minimal) [0]
iiim  = Rotor     (transformFromLanguage [B, A, D, C] Minimal) [0]
ukwm  = Reflector (transformFromLanguage [D, C, B, A] Minimal) [0]

e = Enigma (Plugboard idTransform) (Cartridge [ic, iic, iiic] ukw [0,0,0])
em =  Enigma (Plugboard mplug) (Cartridge [im, iim, iiim] ukwm [0,0,0])
m3Navy = Enigma (Plugboard idTransform) (Cartridge [i, ii, iii] reflectorB [0,0,0])

donitzPlugboard = Plugboard (transformFromLanguage (readLetters "EFMQABGUINKXCJORDPZTHWVLYS") English)

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
    monadicEncrypt c@(Cartridge rotors reflector positions) = total rotors positions
        where
            n = letters . getLanguage $ c
            --total :: (Cipher s) => [s] -> (Letter -> Letter)
            total [] _  = monadicEncrypt reflector
            --total [] __ = monadicEncrypt reflector ---XXX WRONG!
            total _ []  = monadicEncrypt reflector ---XXX WRONG!
            total (r:rs) (p:ps) = eout . total rs ps . ein
                where
                    ein  = \x -> monadicShiftLetter (monadicEncrypt r $ monadicShiftLetter x p n) (-p) n
                    eout = \x -> monadicShiftLetter (monadicDecrypt r $ monadicShiftLetter x p n) (-p) n
            --might step wrong way!
    monadicDecrypt = monadicEncrypt

getNotchesFromRotors [] = []
getNotchesFromRotors ((Rotor _ notches):rs) = notches:getNotchesFromRotors rs
getNotchesFromRotors ((Reflector _ notches):rs) = notches:getNotchesFromRotors rs

getNotchesFromCartridge (Cartridge rs r _) = getNotchesFromRotors rs



----------------------------------------------
--              ENIGMA                      --
----------------------------------------------    
data Enigma l = Enigma (Plugboard l) (Cartridge l) deriving (Eq)
instance HasLanguage Enigma where
    getLanguage (Enigma _ c) = getLanguage c
instance (Language l) => Show (Enigma l) where
    show e@(Enigma p c) = "Enigma(" ++ show (getLanguage e) ++ ")\n" ++
                        "ID  POS                                 NOTCH\n" ++
                        "          ABCDEFGHIJKLMNOPQRSTUVWXYZ\n" ++
                        show p ++ "\n" ++
                        show c
instance Cipher Enigma where
    monadicEncrypt (Enigma p c) = monadicDecrypt p . monadicEncrypt (stepCartridge c) . monadicEncrypt p
    monadicDecrypt = monadicEncrypt
instance PolyalphabeticCipher Enigma where
    monadicPolyEncrypt m e@(Enigma p c) = (monadicEncrypt e m, step e)
    monadicPolyDecrypt = monadicPolyEncrypt


mkEnigma plugBoard rotors reflector rss rps = Enigma plugBoard (Cartridge rotors reflector positions)
    where
        n = letters . getLanguage $ plugBoard
        rssList   = map fromEnum $ readLetters rss
        rpsList   = map fromEnum $ readLetters rps
        positions = reverse $ (\x y -> mod (x-y) n) <$> rpsList <*> rssList
--mkEnigma donitzPlugboard donitzRotors thinReflectorC donitzRingSetting donitzRotorPosition


stepn e 0 = e
stepn e n = step (stepn e (n-1))

--encryptText e txt = zipWith (\ t i -> encrypt (stepn e i) t) (readLetters txt) [1..]
-----------------------------------------------
--              TESTING                      --
----------------------------------------------- 
--check validity
--helper functions
safeHead [] = Nothing
safeHead (x:xs) = Just x

--find :: (Eq a) => a -> [a] -> Maybe a
findValue xs i = isJust (findIndex xs i)
findIndex xs i = safeHead (dropWhile (/= i) xs)

noRepeats [] = True
noRepeats (x:xs) = not (findValue xs x) && noRepeats xs

--a transform is onto if each row has exactly one element(ie, the value 1)
isOnto (Transform m _) = det m /= 0

inverse (Transform t _) = Transform (tr t)

--validRotor :: Rotor -> Bool
validRotor (Rotor t no) = isOnto t &&
--                                    all (== 1) (map (\m) t) 
                                      noRepeats no &&
                                      all (< n) no
    where
        n = length no
validRotor (Reflector t no) = validRotor (Rotor t no)
--                                isSymmetricTransformation t



-------------------------------------------------------------------
--                       ENCRYPTION                              --
-------------------------------------------------------------------
--encrypt (Enigma (Plugboard plugging)(Cartridge rotors reflector positions)) l = 0

step (Enigma plugging c) = Enigma plugging (stepCartridge c)

--stepCartridge :: (Letter l) => Cartridge l -> Cartridge l
stepCartridge c@(Cartridge rotors reflector positions) = Cartridge rotors reflector $ stepHelper positions (getNotchesFromRotors rotors) 1
    where
        n = letters . getLanguage $ c
        --increase position if p is in ps, or if previous was in ps
        stepHelper [] [] _ = []
        stepHelper (p:ps) (ns:nss) carry =
            if elem p ns || carry==1
                then mod (p+1) n:stepHelper ps nss (if p `elem` ns then 1 else 0)
                else p:stepHelper ps nss 0




encrypt cipher p = runIdentity $ monadicEncrypt cipher (Identity p)
encryptText cipher = map (encrypt cipher)

--encryptSeveral = sequence (map (\x -> state $ monadicPolyEncrypt (Identity x)) [A,A,A,A,A,A,A,A])
--then runstate encryptseveral $ starting_enigma