import Criterion.Main
import Cartridge 
import Cipher()
import Enigma
import Language
import Parts
import Control.Monad.State.Strict
import Plugboard

donitzCipherText :: String
donitzCipherText = "LANOTCTOUARBBFPMHPHGCZXTDYGAHGUFXGEWKBLKGJWLQXXTGPJJAVTOCKZFSLPPQIHZFX" ++
                              "OEBWIIEKFZLCLOAQJULJOYHSSMBBGWHZANVOIIPYRBRTDJQDJJOQKCXWDNBBTYVXLYTAPG" ++
                              "VEATXSONPNYNQFUDBBHHVWEPYEYDOHNLXKZDNWRHDUWUJUMWWVIIWZXIVIUQDRHYMNCYEF" ++
                              "UAPNHOTKHKGDNPSAKNUAGHJZSMJBMHVTREQEDGXHLZWIFUSKDQVELNMIMITHBHDBWVHDFY" ++
                              "HJOQIHORTDJDBWXEMEAYXGYQXOHFDMYUXXNOJAZRSGHPLWMLRECWWUTLRTTVLBHYOORGLG" ++
                              "OWUXNXHMHYFAACQEKTHSJW"

main :: IO ()
main = do 
  let cartridge = Cartridge [viii,vi,v,beta] thinReflectorC [0,0,0]
  let plugboard = mkPlugboard [(A,E),(B,F),(C,M),(D,Q),(H,U),(J,N),(L,X),(P,R),(S,Z),(V,W),(G,G),(I,I),(K,K),(O,O),(T,T),(Y,Y)]
  let enigma    = Enigma plugboard cartridge
  defaultMain [ 
    bgroup "encryption" [ 
      bench "donitz"  $ whnf (evalState (encryptText $ stringToLanguage donitzCipherText)) enigma,
      bench "long text"  $ whnf (evalState (encryptText $ stringToLanguage (replicate 1000 'A'))) enigma,
      bench "totalmanual"  $ whnf (evalState (encryptText [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z])) enigma
      --bench "total"  $ whnf (evalState (getTransformFromEnigma)) enigma
      ] ]