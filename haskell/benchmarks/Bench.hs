
{-# LANGUAGE NoMonomorphismRestriction #-}

import Criterion.Main ( bench, bgroup, whnf, defaultMain )
import Cartridge ( Cartridge(Cartridge) )
import Cipher()
import Enigma ( EnigmaState(Enigma), encryptText )
import Language
    ( LetterOrdinal(Z, A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R,
             S, T, U, V, W, X, Y),
      stringToLanguage,
      Letter4, Letter6, EnglishLetter )
import Parts
    ( simple6, viii, vi, v, thinReflectorC, beta, simple4, donitz )
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
  let enigmaMod26   = Enigma plugboard cartridge :: EnigmaState EnglishLetter
  let enigmaOrdinal = Enigma plugboard cartridge :: EnigmaState LetterOrdinal

  --let enigma    = Enigma plugboard cartridge :: EnigmaState EnglishLetter
  let repeated  = stringToLanguage (replicate 100000 'A')
  defaultMain [
    bgroup "encryption" [
      bgroup "Z mod 26" [
        bench "donitz"  $ whnf (evalState (encryptText $ stringToLanguage donitzCipherText)) enigmaMod26,
        bench "long text"  $ whnf (evalState (encryptText repeated)) enigmaMod26,
        bench "totalmanual"  $ whnf (evalState (encryptText [minBound .. maxBound])) enigmaMod26
      ],
      bgroup "LetterOrdinal" [
        bench "donitz"  $ whnf (evalState (encryptText $ stringToLanguage donitzCipherText)) enigmaOrdinal,
        bench "long text"  $ whnf (evalState (encryptText repeated)) enigmaOrdinal,
        bench "totalmanual"  $ whnf (evalState (encryptText [minBound .. maxBound])) enigmaOrdinal
      ]

    ],

    wiringBenchmarks,
    

    bgroup "tracking" [
      bench "tracking of simple4"  $ whnf trackManually (simple4 :: EnigmaState Letter4),
      bench "tracking of simple6"  $ whnf trackManually (simple6 :: EnigmaState Letter6),  --,
      bench "tracking of donitz"  $ whnf trackManually donitz
      ]
    ]