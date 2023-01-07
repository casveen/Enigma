{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module NamedParts where

import Enigma ( mkEnigma, EnigmaState(Enigma) ) --(transformFromLanguage, readLetters, , , Plugboard, Rotor, Enigma, Cartridge)
import Rotor ( Rotor(Reflector, Rotor, NamedRotor) )
import Plugboard ( Plugboard(Plugboard), mkPlugboard )
import Transform ( transformFromLanguage, transformFromOrdinals, idTransform )
import Language( readLetters, LetterOrdinal(..), EnglishLetter)
import Cartridge ( Cartridge(Cartridge) )

reEnum :: (Enum a, Enum b) => a -> b
reEnum = toEnum . fromEnum

-----------------------------------------------------------------------
--                          ENIGMA WHEELS                            --
-----------------------------------------------------------------------
--                            n = 26                                 --
-----------------------------------------------------------------------
ic   = NamedRotor (Rotor (transformFromLanguage (readLetters "DMTWSILRUYQNKFEJCAZBPGXOHV") ) [0]) "COMMERCIAL I"
iic  = NamedRotor (Rotor     (transformFromLanguage (readLetters "HQZGPJTMOBLNCIFDYAWVEUSRKX") ) [0]) "COMMERCIAL II"
iiic = NamedRotor (Rotor     (transformFromLanguage (readLetters "UQNTLSZFMREHDPXKIBVYGJCWOA") ) [0]) "COMMERCIAL III"
ukw  = NamedRotor (Reflector (transformFromLanguage (readLetters "QYHOGNECVPUZTFDJAXWMKISRBL") ) [0]) "UMKEHRWALSE"
                                                                       
i    = NamedRotor (Rotor     (transformFromLanguage (readLetters "EKMFLGDQVZNTOWYHXUSPAIBRCJ") ) [reEnum Q]) "I"
ii   = NamedRotor (Rotor     (transformFromLanguage (readLetters "AJDKSIRUXBLHWTMCQGZNPYFVOE") ) [reEnum E]) "II"
iii  = NamedRotor (Rotor     (transformFromLanguage (readLetters "BDFHJLCPRTXVZNYEIWGAKMUSQO") ) [reEnum V]) "III"
iv   = NamedRotor (Rotor     (transformFromLanguage (readLetters "ESOVPZJAYQUIRHXLNFTGKDCMWB") ) [reEnum J]) "IV"
v    = NamedRotor (Rotor     (transformFromLanguage (readLetters "VZBRGITYUPSDNHLXAWMJQOFECK") ) [reEnum Z]) "V"
vi   = NamedRotor (Rotor     (transformFromLanguage (readLetters "JPGVOUMFYQBENHZRDKASXLICTW") ) [reEnum Z, reEnum M]) "VI"
vii  = NamedRotor (Rotor     (transformFromLanguage (readLetters "NZJHGRCXMYSWBOUFAIVLPEKQDT") ) [reEnum Z, reEnum M]) "VII"
viii = NamedRotor (Rotor     (transformFromLanguage (readLetters "FKQHTLXOCBJSPDZRAMEWNIUYGV") ) [reEnum Z, reEnum M]) "VIII"
beta  = NamedRotor (Rotor (transformFromLanguage (readLetters "LEYJVCNIXWPBQMDRTAKZGFUHOS") ) []) "beta"
gamma = NamedRotor (Rotor (transformFromLanguage (readLetters "FSOKANUERHMBTIYCWLQPZXVGJD") ) []) "gamma"


reflectorA = NamedRotor (Reflector (transformFromLanguage (readLetters "EJMZALYXVBWFCRQUONTSPIKHGD") ) []) "REFLECTOR A"
reflectorB = NamedRotor (Reflector (transformFromLanguage (readLetters "YRUHQSLDPXNGOKMIEBFZCWVJAT") ) []) "REFLECTOR B"
reflectorC = NamedRotor (Reflector (transformFromLanguage (readLetters "FVPJIAOYEDRZXWGCTKUQSBNMHL") ) []) "REFLECTOR C"
thinReflectorB = NamedRotor (Reflector (transformFromLanguage (readLetters "ENKQAUYWJICOPBLMDXZVFTHRGS") ) []) "THIN REFLECTOR B"
thinReflectorC = NamedRotor (Reflector (transformFromLanguage (readLetters "RDOBJNTKVEHMLFCWZAXGYIPSUQ") ) []) "THIN REFLECTOR C"

-----------------------------------------------------------------------
--                            n = 4                                  --
-----------------------------------------------------------------------
mplug =            transformFromOrdinals [A, B, C, D] 
im    = NamedRotor (Rotor     (transformFromOrdinals [B, C, D, A] ) [0]) "I_4"
iim   = NamedRotor (Rotor     (transformFromOrdinals [C, D, A, B] ) [0]) "II_4"
iiim  = NamedRotor (Rotor     (transformFromOrdinals [B, A, D, C] ) [0]) "III_4"
ukwm  = NamedRotor (Reflector (transformFromOrdinals [D, C, B, A] ) [0]) "UKW_4"

-----------------------------------------------------------------------
--                            n = 6                                  --
-----------------------------------------------------------------------
mplug6 =            transformFromOrdinals [A, B, C, D, E, F]
im6    = NamedRotor (Rotor     (transformFromOrdinals [B, C, D, A, F, E]) [0] ) "I_6"
iim6   = NamedRotor (Rotor     (transformFromOrdinals [C, D, A, B, F, E] ) [0]) "II_6"
iiim6  = NamedRotor (Rotor     (transformFromOrdinals [B, A, E, F, C, D] ) [0]) "III_6"
ukwm6  = NamedRotor (Reflector (transformFromOrdinals [F, E, D, C, B, A] ) [0]) "UKW_6"

identityPlugboard = Plugboard idTransform


------------------------------------------------------------------------
--                                   COLLECTIONS                      --
------------------------------------------------------------------------
allPlugboards26 = [Plugboard idTransform]
allRotors26 = [i, ii, iii, iv, v, vi, vii, viii, ic, iic, iiic, beta, gamma]
allReflectors26 = [ukw, reflectorA, reflectorB, reflectorC, thinReflectorB, thinReflectorC]
allRotors4 = [im, iim, iiim, ukwm]
allReflectors4 = [ukwm]
allRotors6 = [im6, iim6, iiim6, ukwm6]
allReflectors6 = [ukwm6]


-----------------------------------------------------------------------
--                       Specific Enigmas                            --
-----------------------------------------------------------------------
simple4 =     let 
        cartridge = Cartridge [im, iim, iiim] ukwm [0,0,0]
        plugboard = mkPlugboard [(A,A),(B,B),(C,C),(D,D)]
    in 
        Enigma plugboard cartridge

simple6 =     let 
        cartridge = Cartridge [im6, iim6, iiim6] ukwm6 [0,0,0,0]
        plugboard = mkPlugboard [(A,A),(B,B),(C,C),(D,D),(E,E),(F,F)]
    in 
        Enigma plugboard cartridge
e = mkEnigma (Plugboard idTransform) [ic, iic, iiic] ukw [0,0,0] [0,0,0]
em =  mkEnigma (Plugboard mplug) [im] ukwm [0,0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0,0]
e6 =  mkEnigma (Plugboard mplug6) [im6, iim6, iiim6] ukwm6 [0,0,0,0,0,0,0,0,0,0] [0,0,0,0,0,0,0,0,0,0]

m3Navy = mkEnigma (Plugboard idTransform) [i, ii, iii] reflectorB [0,0,0] [0,0,0]

--specific type as so to not have to specify it at every usage site, Letter 26 is reasonable
donitz :: EnigmaState EnglishLetter
--donitz :: (Cipherable l) => EnigmaState l
donitz = 
    let 
        cartridge = Cartridge [viii,vi,v,beta] thinReflectorC (map toEnum [5,2,6,3])
        plugboard = mkPlugboard [(A,E),(B,F),(C,M),(D,Q),(H,U),(J,N),(L,X),(P,R),(S,Z),(V,W),(G,G),(I,I),(K,K),(O,O),(T,T),(Y,Y)]
    in 
        Enigma plugboard cartridge