module Parts where

import Enigma --(transformFromLanguage, readLetters, , , Plugboard, Rotor, Enigma, Cartridge)
import Rotor
import Plugboard
import Transform
import Language

-----------------------------------------------------------------------
--                          ENIGMA WHEELS                            --
-----------------------------------------------------------------------
--                            n = 26                                 --
-----------------------------------------------------------------------
idTransform =     transformFromLanguage (readLetters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") 
ic   = Rotor     (transformFromLanguage (readLetters "DMTWSILRUYQNKFEJCAZBPGXOHV") ) [0]
iic  = Rotor     (transformFromLanguage (readLetters "HQZGPJTMOBLNCIFDYAWVEUSRKX") ) [0]
iiic = Rotor     (transformFromLanguage (readLetters "UQNTLSZFMREHDPXKIBVYGJCWOA") ) [0]
ukw  = Reflector (transformFromLanguage (readLetters "QYHOGNECVPUZTFDJAXWMKISRBL") ) [0]
                                                                                                            
i    = Rotor     (transformFromLanguage (readLetters "EKMFLGDQVZNTOWYHXUSPAIBRCJ") ) [fromEnum Q]
ii   = Rotor     (transformFromLanguage (readLetters "AJDKSIRUXBLHWTMCQGZNPYFVOE") ) [fromEnum E]
iii  = Rotor     (transformFromLanguage (readLetters "BDFHJLCPRTXVZNYEIWGAKMUSQO") ) [fromEnum V]
iv   = Rotor     (transformFromLanguage (readLetters "ESOVPZJAYQUIRHXLNFTGKDCMWB") ) [fromEnum J]
v    = Rotor     (transformFromLanguage (readLetters "VZBRGITYUPSDNHLXAWMJQOFECK") ) [fromEnum Z]
vi   = Rotor     (transformFromLanguage (readLetters "JPGVOUMFYQBENHZRDKASXLICTW") ) [fromEnum Z, fromEnum M]
vii  = Rotor     (transformFromLanguage (readLetters "NZJHGRCXMYSWBOUFAIVLPEKQDT") ) [fromEnum Z, fromEnum M]
viii = Rotor     (transformFromLanguage (readLetters "FKQHTLXOCBJSPDZRAMEWNIUYGV") ) [fromEnum Z, fromEnum M]
beta  = Rotor (transformFromLanguage (readLetters "LEYJVCNIXWPBQMDRTAKZGFUHOS") ) []
gamma = Rotor (transformFromLanguage (readLetters "FSOKANUERHMBTIYCWLQPZXVGJD") ) []


reflectorA = Reflector (transformFromLanguage (readLetters "EJMZALYXVBWFCRQUONTSPIKHGD") ) []
reflectorB = Reflector (transformFromLanguage (readLetters "YRUHQSLDPXNGOKMIEBFZCWVJAT") ) []
reflectorC = Reflector (transformFromLanguage (readLetters "FVPJIAOYEDRZXWGCTKUQSBNMHL") ) []
thinReflectorB = Reflector (transformFromLanguage (readLetters "ENKQAUYWJICOPBLMDXZVFTHRGS") ) []
thinReflectorC = Reflector (transformFromLanguage (readLetters "RDOBJNTKVEHMLFCWZAXGYIPSUQ") ) []



-----------------------------------------------------------------------
--                            n = 4                                  --
-----------------------------------------------------------------------
mplug =            transformFromLanguage [A, B, C, D] 
im    = Rotor     (transformFromLanguage [B, C, D, A] ) [0]
iim   = Rotor     (transformFromLanguage [C, D, A, B] ) [0]
iiim  = Rotor     (transformFromLanguage [B, A, D, C] ) [0]
ukwm  = Reflector (transformFromLanguage [D, C, B, A] ) [0]

-----------------------------------------------------------------------
--                            n = 6                                  --
-----------------------------------------------------------------------
mplug6 =            transformFromLanguage [A, B, C, D, E, F]
im6    = Rotor     (transformFromLanguage [B, C, D, A, F, E]) [0]
iim6   = Rotor     (transformFromLanguage [C, D, A, B, F, E] ) [0]
iiim6  = Rotor     (transformFromLanguage [B, A, E, F, C, D] ) [0]
ukwm6  = Reflector (transformFromLanguage [F, E, D, C, B, A] ) [0]

e = mkEnigma (Plugboard idTransform) [ic, iic, iiic] ukw "AAA" "AAA"
em =  mkEnigma (Plugboard mplug) [im] ukwm "AAAAAAAAAA" "AAAAAAAAAA"
e6 =  mkEnigma (Plugboard mplug6) [im6] ukwm6 "AAAAAAAAAA" "AAAAAAAAAA"

m3Navy = mkEnigma (Plugboard idTransform) [i, ii, iii] reflectorB "AAA" "AAA"

donitzPlugboard = Plugboard (transformFromLanguage (readLetters "EFMQABGUINKXCJORDPZTHWVLYS") )
donitzRotors = [ii,iii,v,v,i,iii,iv,ii,iii,i,ii] --NO
donitzRingSetting = "AAAAAAAAAAA" --NO
donitzRotorPosition = "AAAAAAAAAA" --no

identityPlugboard = (Plugboard idTransform)

allPlugboards26 = [(Plugboard idTransform)]
allRotors26 = [i, ii, iii, iv, v, vi, vii, viii, ic, iic, iiic, beta, gamma]
allReflectors26 = [ukw, reflectorA, reflectorB, reflectorC, thinReflectorB, thinReflectorC]
allRotors4 = [im, iim, iiim, ukwm]
allReflectors4 = [ukwm]
allRotors6 = [im6, iim6, iiim6, ukwm6]
allReflectors6 = [ukwm6]
