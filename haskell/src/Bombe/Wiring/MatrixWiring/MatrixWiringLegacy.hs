module Bombe.Wiring.MatrixWiring.MatrixWiringLegacy(
MatrixWiringLegacy(..)
) where

import Bombe.Wiring.MatrixWiring.MatrixWiring (WM, MatrixWiring(..))
import Bombe.Wiring.Wiring (Wiring(..), bundleToWire, wireToBundleWire)
import Numeric.LinearAlgebra (atIndex, accum, assoc, step)
import Bombe.Wiring.TransitiveClosure (transitiveClosure)

data MatrixWiringLegacy = MatrixWiringLegacy WM Int deriving (Show)

instance Wiring MatrixWiringLegacy where
    getLetters (MatrixWiringLegacy _ n) = n

    isConnectedBW (MatrixWiringLegacy m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            m `atIndex` (i,j) > 0

    isConnected (MatrixWiringLegacy m _) i j = m `atIndex` (i,j) > 0

    connectWire (MatrixWiringLegacy m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            MatrixWiringLegacy (step (accum m (+) [
                ((i,j),1),
                ((j,i),1)
                ])) n

    initialize n =
        let
            allIndexes =
                [((i, j), 1) | i <- [0 .. (n * n - 1)],
                               let (bi, wi) = wireToBundleWire i n,
                               j <- [0 .. (n * n - 1)],
                               let (bj, wj) = wireToBundleWire j n,
                               --i <= j,
                               i == j || bi == wj && bj == wi]
            matrix =
                assoc
                    (n*n, n*n)
                    0
                    allIndexes
        in
            MatrixWiringLegacy matrix n

    closure (MatrixWiringLegacy m n) = MatrixWiringLegacy (transitiveClosure m) n

instance MatrixWiring MatrixWiringLegacy where

    getMatrix (MatrixWiringLegacy m _) = m

    memoizedClosure = undefined