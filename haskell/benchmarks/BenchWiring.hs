module BenchWiring (
    wiringBenchmarks
) where
import Criterion.Main ( bench, bgroup, whnf, Benchmark )
import Cartridge ()
import Cipher()
import Data.Map (empty)
import Numeric.LinearAlgebra ( sumElements )
import Bombe.Wiring.MatrixWiring.MatrixWiringStandard
    ( MatrixWiringStandard )
import Bombe.Wiring.MatrixWiring.MatrixWiringLegacy
    ( MatrixWiringLegacy )
import Bombe.Wiring.MatrixWiring.MatrixWiringCompressed
    ( MatrixWiringCompressed )
import Bombe.Wiring.Wiring
    ( wireToBundleWire,
      Wiring(closure, initialize, getLetters, connectWire) )
import Bombe.Wiring.MatrixWiring.MatrixWiring
    ( MatrixWiring(getMatrix, memoizedClosure), Mem )
import Numeric.LinearAlgebra.Data (I)


wiringBenchmarks :: Benchmark
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
            bench "MatrixWiring"  $ whnf (fst . repeatedBenchesMemoized) (initialize 4 :: MatrixWiringStandard),
            bench "MatrixWiringLegacy" $ whnf (fst . repeatedBenchesMemoized) (initialize 4 :: MatrixWiringLegacy),
            bench "MatrixWiringCompressed" $ whnf (fst . repeatedBenchesMemoized) (initialize 4 :: MatrixWiringCompressed)
        ]
    ]


connectAll :: Wiring a => a -> a
connectAll m =
    let
        n = getLetters m
    in
        foldl
            (\acc (i,j) -> connectWire acc i j)
            m
            [(wireToBundleWire i n, wireToBundleWire j n) | i <- [0..(n*n-1)], j <- [i..(n*n-1)]]

connectBidiagonal :: Wiring a => a -> a
connectBidiagonal m =
    let
        n = getLetters m
    in
        foldl
            (\acc (i,j) -> connectWire acc i j)
            m
            [(wireToBundleWire i n, wireToBundleWire (i+1) n) | i <- [0..(n*n-2)]]

connectRandom :: Wiring a => Int -> a -> a
connectRandom k m =
    let
        n = getLetters m
    in
        foldl
            (\acc (i,j) -> connectWire acc i j)
            m
            [(wireToBundleWire i n, wireToBundleWire (mod (i+j*i*j*k+j + k) (n*n)) n) | i <- [0..(n*n-2)], j<-[0..n]]

repeatedBenchesMemoized :: MatrixWiring a => a -> (I, Mem)
repeatedBenchesMemoized m =
    foldl
        (\(acc,previousMemo) k ->
            let
                (closed, newMemo) = memoizedClosure (connectRandom k m, previousMemo)
            in
                (acc + sumElements (getMatrix closed), newMemo)
        )
        (0, empty)
        [0..100]

repeatedBenchesUnmemoized :: MatrixWiring b => b -> I
repeatedBenchesUnmemoized m =
    foldl
        (\acc k ->
            acc + sumElements (getMatrix . closure . connectRandom k $ m)
        )
        0
        [0..10]