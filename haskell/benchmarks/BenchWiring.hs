module BenchWiring where
import Criterion.Main
import Cartridge
import Cipher()
--import Enigma hiding(initialize)
import Language
import Parts
import Control.Monad.State.Strict
import Plugboard
import Bombe.Wiring.Wiring
import Bombe.Tracker (trackManually)
import Data.Map (empty)
import Numeric.LinearAlgebra
import Bombe.Wiring.MatrixWiring.MatrixWiringStandard
import Bombe.Wiring.MatrixWiring.MatrixWiringLegacy
import Bombe.Wiring.MatrixWiring.MatrixWiringCompressed
import Bombe.Wiring.Wiring
import Bombe.Wiring.MatrixWiring.MatrixWiring


wiringBenchmarks =
    bgroup "wiring" $ [
        bgroup "initialization" [
            bench "MatrixWiring"  $ whnf (initialize :: Int -> MatrixWiringStandard) 26,
            bench "MatrixWiringLegacy" $ whnf (initialize :: Int -> MatrixWiringLegacy) 26,
            bench "MatrixWiringCompressed" $ whnf (initialize :: Int -> MatrixWiringCompressed) 26
        ],

        bgroup "wire all" [
            bench "MatrixWiring"  $ whnf connectAll (initialize 26 :: MatrixWiringStandard),
            bench "MatrixWiringLegacy" $ whnf connectAll (initialize 26 :: MatrixWiringLegacy),
            bench "MatrixWiringCompressedompressed" $ whnf connectAll (initialize 26 :: MatrixWiringCompressed)
        ],

        bgroup "transitive closure of bidiagonal" [
            bench "MatrixWiring"  $ whnf (closure . connectBidiagonal) (initialize 26 :: MatrixWiringStandard),
            bench "MatrixWiringLegacy" $ whnf (closure . connectBidiagonal) (initialize 26 :: MatrixWiringLegacy),
            bench "MatrixWiringCompressed" $ whnf (closure . connectBidiagonal) (initialize 26 :: MatrixWiringCompressed)
        ],

        bgroup "transitive closure of haphazard" [
            bench "MatrixWiring"  $ whnf (closure . connectRandom 1) (initialize 26 :: MatrixWiringStandard),
            bench "MatrixWiringLegacy" $ whnf (closure . connectRandom 1) (initialize 26 :: MatrixWiringLegacy),
            bench "MatrixWiringCompressed" $ whnf (closure . connectRandom 1) (initialize 26 :: MatrixWiringCompressed)
        ],

        bgroup "repeated transitive closures" [
            bench "MatrixWiring"  $ whnf (fst . repeatedBenchesMemoized) (initialize 26 :: MatrixWiringStandard),
            bench "MatrixWiringLegacy" $ whnf repeatedBenchesUnmemoized (initialize 26 :: MatrixWiringLegacy),
            bench "MatrixWiringCompressed" $ whnf repeatedBenchesMemoized (initialize 26 :: MatrixWiringCompressed)
        ]
    ]


connectAll m =
    let
        n = getLetters m
    in
        foldl
            (\acc (i,j) -> connectWire acc i j)
            m
            [(wireToBundleWire i n, wireToBundleWire j n) | i <- [0..(n*n-1)], j <- [i..(n*n-1)]]

connectBidiagonal m =
    let
        n = getLetters m
    in
        foldl
            (\acc (i,j) -> connectWire acc i j)
            m
            [(wireToBundleWire i n, wireToBundleWire (i+1) n) | i <- [0..(n*n-2)]]

connectRandom k m =
    let
        n = getLetters m
    in
        foldl
            (\acc (i,j) -> connectWire acc i j)
            m
            [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j*k+j + k) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]

repeatedBenchesMemoized m =
    foldl
        (\(acc,previousMemo) k ->
            let
                (closed, newMemo) = memoizedClosure (connectRandom k m, previousMemo)
            in
                (acc + sumElements (getMatrix closed), newMemo)
        )
        (0, empty)
        [0..10]

repeatedBenchesUnmemoized m =
    foldl
        (\acc k ->
            acc + sumElements (getMatrix . closure . connectRandom k $ m)
        )
        0
        [0..10]