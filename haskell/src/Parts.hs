{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Parts where

import Enigma ( mkEnigma, EnigmaState(Enigma) ) --(transformFromLanguage, readLetters, , , Plugboard, Rotor, Enigma, Cartridge)
import Rotor ( Rotor(Reflector, Rotor) )
import Plugboard ( Plugboard(Plugboard), mkPlugboard )
import Transform ( transformFromLanguage, transformFromOrdinals, idTransform )
import Language( readLetters, LetterOrdinal(..), Letter, EnglishLetter)
import Cartridge ( Cartridge(Cartridge) )
import Cipher (Cipherable)

reEnum :: (Enum a, Enum b) => a -> b
reEnum = toEnum . fromEnum

-----------------------------------------------------------------------
--                          ENIGMA WHEELS                            --
-----------------------------------------------------------------------
--                            n = 26                                 --
-----------------------------------------------------------------------
--idTransform :: (Enum e, Ord e) => Transform e
--idTransform = fromList    --transformFromLanguage (readLetters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") 

--ic :: Rotor (Letter 26)
ic   = Rotor     (transformFromLanguage (readLetters "DMTWSILRUYQNKFEJCAZBPGXOHV") ) [0]
--iic :: Rotor (Letter 26)
iic  = Rotor     (transformFromLanguage (readLetters "HQZGPJTMOBLNCIFDYAWVEUSRKX") ) [0]
--iiic :: Rotor (Letter 26)
iiic = Rotor     (transformFromLanguage (readLetters "UQNTLSZFMREHDPXKIBVYGJCWOA") ) [0]
--ukw :: Rotor (Letter 26)
ukw  = Reflector (transformFromLanguage (readLetters "QYHOGNECVPUZTFDJAXWMKISRBL") ) [0]

--i :: (Enum e, Ord e) => Rotor e                                                                             
i    = Rotor     (transformFromLanguage (readLetters "EKMFLGDQVZNTOWYHXUSPAIBRCJ") ) [reEnum Q]
--ii :: Rotor (Letter 26)
ii   = Rotor     (transformFromLanguage (readLetters "AJDKSIRUXBLHWTMCQGZNPYFVOE") ) [reEnum E]
--iii :: Rotor (Letter 26)
iii  = Rotor     (transformFromLanguage (readLetters "BDFHJLCPRTXVZNYEIWGAKMUSQO") ) [reEnum V]
--iv :: Rotor (Letter 26)
iv   = Rotor     (transformFromLanguage (readLetters "ESOVPZJAYQUIRHXLNFTGKDCMWB") ) [reEnum J]
--v :: Rotor (Letter 26)
v    = Rotor     (transformFromLanguage (readLetters "VZBRGITYUPSDNHLXAWMJQOFECK") ) [reEnum Z]
--vi :: Rotor (Letter 26)
vi   = Rotor     (transformFromLanguage (readLetters "JPGVOUMFYQBENHZRDKASXLICTW") ) [reEnum Z, reEnum M]
--vii :: Rotor (Letter 26)
vii  = Rotor     (transformFromLanguage (readLetters "NZJHGRCXMYSWBOUFAIVLPEKQDT") ) [reEnum Z, reEnum M]
--viii :: Rotor (Letter 26)
viii = Rotor     (transformFromLanguage (readLetters "FKQHTLXOCBJSPDZRAMEWNIUYGV") ) [reEnum Z, reEnum M]
--beta :: (Enum e, Ord e) => Rotor e
beta  = Rotor (transformFromLanguage (readLetters "LEYJVCNIXWPBQMDRTAKZGFUHOS") ) []
--gamma :: Rotor (Letter 26)
gamma = Rotor (transformFromLanguage (readLetters "FSOKANUERHMBTIYCWLQPZXVGJD") ) []


--reflectorA :: Rotor (Letter 26)
reflectorA = Reflector (transformFromLanguage (readLetters "EJMZALYXVBWFCRQUONTSPIKHGD") ) []
--reflectorB :: Rotor (Letter 26)
reflectorB = Reflector (transformFromLanguage (readLetters "YRUHQSLDPXNGOKMIEBFZCWVJAT") ) []
--reflectorC :: Rotor (Letter 26)
reflectorC = Reflector (transformFromLanguage (readLetters "FVPJIAOYEDRZXWGCTKUQSBNMHL") ) []
--thinReflectorB :: Rotor (Letter 26)
thinReflectorB = Reflector (transformFromLanguage (readLetters "ENKQAUYWJICOPBLMDXZVFTHRGS") ) []
--thinReflectorC :: Rotor (Letter 26)
thinReflectorC = Reflector (transformFromLanguage (readLetters "RDOBJNTKVEHMLFCWZAXGYIPSUQ") ) []

-----------------------------------------------------------------------
--                            n = 4                                  --
-----------------------------------------------------------------------
mplug =            transformFromOrdinals [A, B, C, D] 
im    = Rotor     (transformFromOrdinals [B, C, D, A] ) [0]
iim   = Rotor     (transformFromOrdinals [C, D, A, B] ) [0]
iiim  = Rotor     (transformFromOrdinals [B, A, D, C] ) [0]
ukwm  = Reflector (transformFromOrdinals [D, C, B, A] ) [0]

-----------------------------------------------------------------------
--                            n = 6                                  --
-----------------------------------------------------------------------
mplug6 =            transformFromOrdinals [A, B, C, D, E, F]
im6    = Rotor     (transformFromOrdinals [B, C, D, A, F, E]) [0]
iim6   = Rotor     (transformFromOrdinals [C, D, A, B, F, E] ) [0]
iiim6  = Rotor     (transformFromOrdinals [B, A, E, F, C, D] ) [0]
ukwm6  = Reflector (transformFromOrdinals [F, E, D, C, B, A] ) [0]


--donitzPlugboard :: Plugboard (Letter 26)
--donitzPlugboard = mkPlugboard [(A,E),(B,F),(C,M),(D,Q),(H,U),(J,N),(L,X),(P,R),(S,Z),(V,W),(G,G),(I,I),(K,K),(O,O),(T,T),(Y,Y)]
--donitzRotors = [ii,iii,v,v,i,iii,iv,ii,iii,i,ii] --NO
--donitzRingSetting = "AAAAAAAAAAA" --NO
--donitzRotorPosition = "AAAAAAAAAA" --no

--identityPlugboard :: (Enum e, Ord e) => Plugboard e
identityPlugboard = Plugboard idTransform


------------------------------------------------------------------------
--                                   COLLECTIONS                      --
------------------------------------------------------------------------
--allPlugboards26 :: [Plugboard (Letter 26)]
--allPlugboards26 :: (Enum e, Ord e) => [Plugboard e]
allPlugboards26 = [Plugboard idTransform]
--allRotors26 :: [Rotor (Letter 26)]
--allRotors26 :: (Enum e, Ord e) => [Rotor e]
allRotors26 = [i, ii, iii, iv, v, vi, vii, viii, ic, iic, iiic, beta, gamma]
--allReflectors26 :: [Rotor (Letter 26)]
allReflectors26 = [ukw, reflectorA, reflectorB, reflectorC, thinReflectorB, thinReflectorC]
--allRotors4 :: [Rotor (Letter 4)]
allRotors4 = [im, iim, iiim, ukwm]
--allReflectors4 :: [Rotor (Letter 4)]
allReflectors4 = [ukwm]
--allRotors6 :: [Rotor (Letter 6)]
allRotors6 = [im6, iim6, iiim6, ukwm6]
--allReflectors6 :: [Rotor (Letter 6)]
allReflectors6 = [ukwm6]


-----------------------------------------------------------------------
--                       Specific Enigmas                            --
-----------------------------------------------------------------------
--simple4 :: EnigmaState (Letter 4)
simple4 =     let 
        cartridge = Cartridge [im, iim, iiim] ukwm [0,0,0]
        plugboard = mkPlugboard [(A,A),(B,B),(C,C),(D,D)]
    in 
        Enigma plugboard cartridge

--simple6 :: EnigmaState (Letter 6)
simple6 =     let 
        cartridge = Cartridge [im6, iim6, iiim6] ukwm6 [0,0,0,0]
        plugboard = mkPlugboard [(A,A),(B,B),(C,C),(D,D),(E,E),(F,F)]
    in 
        Enigma plugboard cartridge
--e :: Maybe (EnigmaState (Letter 26))
e = mkEnigma (Plugboard idTransform) [ic, iic, iiic] ukw [0,0,0] [0,0,0]
--em :: Maybe (EnigmaState (Letter 4))
em =  mkEnigma (Plugboard mplug) [im] ukwm [0,0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0,0]
--e6 :: Maybe (EnigmaState (Letter 6))
e6 =  mkEnigma (Plugboard mplug6) [im6, iim6, iiim6] ukwm6 [0,0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0,0]

--m3Navy :: Maybe (EnigmaState (Letter 26))
m3Navy = mkEnigma (Plugboard idTransform) [i, ii, iii] reflectorB [0,0,0] [0,0,0]

--specific type as so to not have to specify it at every usage site, Letter 26 is reasonable
donitz :: EnigmaState EnglishLetter
--donitz :: (Cipherable l) => EnigmaState l
donitz = 
    let 
        --cartridge :: Cartridge (Letter 26)
        cartridge = Cartridge [viii,vi,v,beta] thinReflectorC (map toEnum [5,2,6,3])
        plugboard = mkPlugboard [(A,E),(B,F),(C,M),(D,Q),(H,U),(J,N),(L,X),(P,R),(S,Z),(V,W),(G,G),(I,I),(K,K),(O,O),(T,T),(Y,Y)]
    in 
        Enigma plugboard cartridge