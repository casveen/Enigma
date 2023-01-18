module Bombe.Wiring.MatrixWiring.MatrixWiringCompressed (
MatrixWiringCompressed(..)
) where

import Data.Map ( Map, (!), fromList )
import Prelude hiding(pi)
import Bombe.Wiring.MatrixWiring.MatrixWiring (WM, MatrixWiring(..))
import Bombe.Wiring.Wiring (Wiring(..), wireToBundleWire)
import Numeric.LinearAlgebra (ident, atIndex, accum)
import Bombe.Wiring.TransitiveClosure (transitiveClosure, transitiveClosureMemoized)
import Debug.Trace


data MatrixWiringCompressed = MatrixWiringCompressed WM Int (Map (Int, Int) Int) deriving(Show ,Eq)

instance Wiring MatrixWiringCompressed where

    getLetters (MatrixWiringCompressed _ n _ ) = n

    isConnectedBW (MatrixWiringCompressed m _ mapping) ii jj =
        let
            i = mapping ! ii
            j = mapping ! jj
        in
            m `atIndex` (i,j) > 0

    isConnected (MatrixWiringCompressed m n mapping) i j =
        let
            ii = wireToBundleWire i n
            jj = wireToBundleWire j n
            pi = mapping ! ii
            pj = mapping ! jj
        in
             m `atIndex` (pi,pj) > 0

    connectWire (MatrixWiringCompressed m n mapping) ii jj =
        let
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

            allIndexes =
                [((bi, wi), (bj, wj)) | i <- [0 .. (n * n - 1)],
                               let (bi, wi) = wireToBundleWire i n,
                               j <- [0 .. (n * n - 1)],
                               let (bj, wj) = wireToBundleWire j n,
                               bi == wj && bj == wi]

            matrix = ident m

            --connectAll :: MatrixWiringCompressed -> MatrixWiringCompressed
            connectAll mm = foldl (\acc (ii,jj) -> connectWire acc ii jj) mm allIndexes

            mapping = fromList $
                [((b,w), i) | b <- [0..(n-1)], 
                              w <- [b..(n-1)],
                              let i = (w-(b*(b+1) `div` 2))+b*n] ++
                [((b,0), b) | b <- [1..(n-1)]] ++
                [((b,w), i) | b <- [1..(n-1)], 
                              w <- [1..(b-1)],
                              let i = b+w*n - (w*(w+1) `div` 2)]
        in
            connectAll (MatrixWiringCompressed matrix n mapping)

    closure (MatrixWiringCompressed m n mapping) = MatrixWiringCompressed (transitiveClosure m) n mapping

    

instance MatrixWiring MatrixWiringCompressed where
    getMatrix (MatrixWiringCompressed m _ _) = m

    memoizedClosure (MatrixWiringCompressed m n mp, memin) =
        let
            (mt, mem) = transitiveClosureMemoized m memin
        in
            (MatrixWiringCompressed mt n mp, mem)