module Bombe.Wiring.MatrixWiring.MatrixWiringStandard where

import Bombe.Wiring.MatrixWiring.MatrixWiring (WM, MatrixWiring(..))
import Bombe.Wiring.Wiring (Wiring(..), bundleToWire, wireToBundleWire)
import Numeric.LinearAlgebra (atIndex, accum, assoc)
import Bombe.Wiring.TransitiveClosure (transitiveClosure, transitiveClosureMemoized)


data MatrixWiringStandard        = MatrixWiringStandard WM Int deriving (Show, Eq)

instance Wiring MatrixWiringStandard where
    getLetters (MatrixWiringStandard _ n) = n

    isConnectedBW (MatrixWiringStandard m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
            
        in
            m `atIndex` (i,j) > 0

    isConnected (MatrixWiringStandard m n) i j = m `atIndex` (i,j) > 0

    connectWire (MatrixWiringStandard m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            MatrixWiringStandard (accum m const [
                ((i,j),1),
                ((j,i),1)
                ]) n

    initialize n =
        let
            allIndexes =
                [((i, j), 1) | i <- [0 .. (n * n - 1)],
                               let (bi, wi) = wireToBundleWire i n,
                               j <- [0 .. (n * n - 1)],
                               let (bj, wj) = wireToBundleWire j n,
                               i == j || bi == wj && bj == wi]
            matrix =
                assoc
                    (n*n, n*n)
                    0
                    allIndexes
        in
            MatrixWiringStandard matrix n

    closure (MatrixWiringStandard m n) = MatrixWiringStandard (transitiveClosure m) n


instance MatrixWiring MatrixWiringStandard where 
    getMatrix (MatrixWiringStandard m _) = m

    

    memoizedClosure (MatrixWiringStandard m n, memin) = 
        let 
            (mt, mem) = transitiveClosureMemoized m memin
        in
            (MatrixWiringStandard mt n, mem)