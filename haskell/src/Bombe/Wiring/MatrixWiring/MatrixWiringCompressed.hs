module Bombe.Wiring.MatrixWiring.MatrixWiringCompressed where

import Data.Map
import Bombe.Wiring.MatrixWiring.MatrixWiring (WM, MatrixWiring(..))
import Bombe.Wiring.Wiring (Wiring(..), wireToBundleWire)
import Numeric.LinearAlgebra (ident, atIndex, accum)
import Bombe.Wiring.TransitiveClosure (transitiveClosure, transitiveClosureMemoized)


data MatrixWiringCompressed = MatrixWiringCompressed WM Int (Map (Int, Int) Int) deriving(Show ,Eq)

instance Wiring MatrixWiringCompressed where

    getLetters (MatrixWiringCompressed _ n _ ) = n

    isConnectedBW (MatrixWiringCompressed m n mapping) ii jj =
        let
            i = mapping ! ii
            j = mapping ! jj
        in
            m `atIndex` (i,j) > 0

    isConnected (MatrixWiringCompressed m n mapping) i j =
        let
            ii = wireToBundleWire i n
            jj = wireToBundleWire j n
            i = mapping ! ii
            j = mapping ! jj
        in
             m `atIndex` (i,j) > 0

    connectWire (MatrixWiringCompressed m n mapping) ii jj =
        let
            swap (x,y) = (y,x)
            i  = mapping ! ii
            j  = mapping ! jj
        in
            MatrixWiringCompressed(accum m const [
                ((i,j),1),
                ((j,i),1)
                ]) n mapping

    initialize n =
        let
            m = div (n*(n+1)) 2

            matrix = ident m

            mapping = fromList
                ([((i,j), n * i - (i*(i+1) `div` 2) + j) | i <- [0 .. (n - 1)],
                                        j <- [i .. (n - 1)]] ++
                [((i,j), n* i - (i*(i+1) `div` 2) + j ) | i <- [1 .. (n - 1)],
                                          j <- [0 .. (i-1)]])
        in
            MatrixWiringCompressed matrix n mapping

    closure (MatrixWiringCompressed m n mapping) = MatrixWiringCompressed(transitiveClosure m) n mapping

    

instance MatrixWiring MatrixWiringCompressed where
    getMatrix (MatrixWiringCompressed m _ _) = m

    memoizedClosure (MatrixWiringCompressed m n mp, memin) =
        let
            (mt, mem) = transitiveClosureMemoized m memin
        in
            (MatrixWiringCompressed m n mp, mem)