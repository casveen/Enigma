import Criterion.Main ( bench, bgroup, whnf, defaultMain )
import Cartridge ( Cartridge(Cartridge) )
import Cipher()
import Enigma ( EnigmaState(Enigma), encryptText )
import Language
    ( Letter(Z, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R,
             S, T, U, V, W, X, Y),
      stringToLanguage )
import Parts
    ( simple6, viii, vi, v, thinReflectorC, beta, simple4 )
import Control.Monad.State.Strict ( evalState )
import Plugboard ( mkPlugboard )
import Bombe.Tracker (trackManually)
import BenchWiring ( wiringBenchmarks )

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
      bench "long text"  $ whnf (evalState (encryptText $ stringToLanguage (replicate 100000 'A'))) enigma,
      bench "totalmanual"  $ whnf (evalState (encryptText [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z])) enigma
      ],
    wiringBenchmarks,
    

    bgroup "tracking" [
      bench "tracking of simple4"  $ whnf trackManually simple4,
      bench "tracking of simple6"  $ whnf trackManually simple6--,
      --bench "tracking of donitz"  $ whnf trackManually donitz
      ]
    ]