import Criterion.Main
import Cartridge
import Cipher()
import Enigma
import Language
import Parts
import Control.Monad.State.Strict
import Plugboard
import Bombe.Wiring
import Bombe.Tracker (trackManually)
import Data.Map (empty)
import Numeric.LinearAlgebra

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

    bgroup "wiring" [
      bench "initialize"  $ whnf (
          Bombe.Wiring.initialize :: Int -> MatrixWiring
        ) 26
      ,
      bench "wireAll-MatrixWiring"  $ whnf (
          let
            connectAll m@(MatrixWiring mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire j n) | i <- [0..(n*n-1)], j <- [i..(n*n-1)]]
          in
            connectAll
        ) (Bombe.Wiring.initialize 8),
        bench "wireAll-MatrixWiring0"  $ whnf (
          let
            connectAll m@(MatrixWiring0 mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire j n) | i <- [0..(n*n-1)], j <- [i..(n*n-1)]]
          in
            connectAll
        ) (Bombe.Wiring.initialize 8),
        bench "transitiveClosureOfBand-MatrixWiring"  $ whnf (
          let
            connectAll m@(MatrixWiring mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (i+1) n) | i <- [0..(n*n-2)]]
          in
            closure . connectAll
        ) (Bombe.Wiring.initialize 26),
        bench "transitiveClosureOfBand-MatrixWiring0"  $ whnf (
          let
            connectAll m@(MatrixWiring0 mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (i+1) n) | i <- [0..(n*n-2)]]
          in
            closure . connectAll
        ) (Bombe.Wiring.initialize 26),
        bench "transitiveClosureOfBand-MatrixWiringC"  $ whnf (
          let
            connectAll m@(MatrixWiringC _ n _) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (i+1) n) | i <- [0..(n*n-2)]]
          in
            closure . connectAll
        ) (Bombe.Wiring.initialize 26),

        bench "transitiveClosureRepeatedOther-MatrixWiring"  $ whnf (
          let
            connectAll m@(MatrixWiring mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j+j) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]
          in
            -- \x -> foldl (\(_,mem) _ -> memoizedClosure . connectAll $ x) (x, empty) [0..100]
            \x -> foldl (\(acc,mem) _ -> 
              let 
                (cl, memm ) = memoizedClosure (connectAll x, mem)
              in 
                (acc + sumElements (getMatrix cl), memm)) 
                (0, empty) [0..100]
        ) (Bombe.Wiring.initialize 12),

        bench "transitiveClosureRepeatedOther-MatrixWiring0"  $ whnf (
          let
            connectAll m@(MatrixWiring0 mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j+j) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]
          in
            \x -> foldl (\acc _ -> acc + sumElements (getMatrix . closure . connectAll $ x)) (0) [0..100]
        ) (Bombe.Wiring.initialize 12),

        bench "transitiveClosureRepeatedOther-MatrixWiringC"  $ whnf (
          let
            connectAll m@(MatrixWiringC mat n _) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j+j) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]
          in
            \x -> foldl (\acc _ -> acc + sumElements (getMatrix . closure . connectAll $ x)) (0) [0..100]
        ) (Bombe.Wiring.initialize 12),





        bench "transitiveClosureOther-MatrixWiring"  $ whnf (
          let
            connectAll m@(MatrixWiring mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j+j) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]
          in
            closure . connectAll
        ) (Bombe.Wiring.initialize 12),
        bench "transitiveClosureOfOther-MatrixWiring0"  $ whnf (
          let
            connectAll m@(MatrixWiring0 mat n) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j+j) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]
          in
            closure . connectAll
        ) (Bombe.Wiring.initialize 12),
        bench "transitiveClosureOfOther-MatrixWiringC"  $ whnf (
          let
            connectAll m@(MatrixWiringC mat n mapping) =
              foldl
                (\acc (i,j) -> connectWire acc i j)
                m
                [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j+j) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]
          in
            closure . connectAll
        ) (Bombe.Wiring.initialize 12)
    ],

    bgroup "tracking" [
      bench "tracking of simple4"  $ whnf trackManually simple4,
      bench "tracking of simple6"  $ whnf trackManually simple6--,
      --bench "tracking of donitz"  $ whnf trackManually donitz
      ]
    ]