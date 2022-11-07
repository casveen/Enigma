module Parts where

import Enigma --(transformFromLanguage, readLetters, English, Minimal, Plugboard, Rotor, Enigma, Cartridge)
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

e = mkEnigma (Plugboard idTransform) [ic, iic, iiic] ukw "AAA" "AAA"
em =  mkEnigma (Plugboard mplug) [im, iim, iiim] ukwm "AAA" "AAA"
m3Navy = mkEnigma (Plugboard idTransform) [i, ii, iii] reflectorB "AAA" "AAA"

donitzPlugboard = Plugboard (transformFromLanguage (readLetters "EFMQABGUINKXCJORDPZTHWVLYS") English)
donitzRotors = [i, ii, iii] --NO
donitzRingSetting = "AAA" --NO
donitzRotorPosition = "AAA" --no