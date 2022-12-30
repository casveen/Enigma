module Bombe.Wiring.TransitiveClosure (
    transitiveClosure, 
    transitiveClosureMemoized
) where

import Bombe.Wiring.MatrixWiring.MatrixWiring (WM, Mem)
import Data.Map(lookup, insert)
import Prelude hiding(lookup)
import Numeric.LinearAlgebra (toBlocksEvery, toLists, rows, cols, step, (|||), (===), Container)
import Data.Maybe (fromMaybe)

matrixOr :: Num a => a -> a -> a
matrixOr m1 m2 = m1 + m2 - m1 * m2 -- XXX problem if not 0,1 matrices!!!
--matrixOr2 :: Num a => a -> a -> a
--matrixOr2 m1 m2 = m1 + m2 -- XXX problem if not 0,1 matrices!!!

(<<>>) :: (Ord e, Container c e, Semigroup (c e)) => c e -> c e -> c e
(<<>>) a b = step $ a <> b

--(<<<>>>) :: Semigroup a => a -> a -> a
--(<<<>>>) a b = a <> b

transitiveClosure :: WM -> WM
transitiveClosure m =
    let
        rn  = rows m
        cn  = cols m

        isUniform =
            let
                flattened = mconcat (toLists m)
            in
                Prelude.foldl (\acc x -> acc && (x == head flattened)) True flattened

        partitionMatrix =
            let
                rs = div rn 2 + mod rn 2 --mod ensures > half
                cs = div cn 2 + mod cn 2 --mod ensures > half
                blocks = toBlocksEvery rs cs m
            in
                ((blocks !! 0 !! 0, blocks !! 0 !! 1),(blocks !! 1 !! 0, blocks !! 1 !! 1))

        ((a,b),(c,d)) = partitionMatrix

        u1 = transitiveClosure d
        u2 = u1 <<>> c
        e  = transitiveClosure (a `matrixOr` (b <<>> u2))
        u3 = b <<>> u1
        f  = e <<>> u3
        g  = u2 <<>> e
        h  = u1 `matrixOr` (u2 <<>> f)
        comp = (e ||| f) === (g ||| h)
        res = if isUniform
            then m
            else step comp
    in
        res

transitiveClosureMemoized :: WM -> Mem -> (WM, Mem)
transitiveClosureMemoized m mem =
    let
        rn  = rows m
        cn  = cols m

        isUniform =
            let
                flattened = mconcat (toLists m)
            in
                Prelude.foldl (\acc x -> acc && (x == head flattened)) True flattened

        partitionMatrix =
            let
                rs = div rn 2 + mod rn 2 --mod ensures > half
                cs = div cn 2 + mod cn 2 --mod ensures > half
                blocks = toBlocksEvery rs cs m
            in
                (blocks !! 0 !! 0, blocks !! 0 !! 1,  blocks !! 1 !! 0, blocks !! 1 !! 1)

        (a, b, c, d) = partitionMatrix

        (u1, mem1) = transitiveClosureMemoized d mem --ok
        u2 = u1 <<>> c --ok
        (e, mem2)  = transitiveClosureMemoized (a `matrixOr` (b <<>> u2)) mem1 --ok, lower is dont care, no influence since no <<>>
        u3 = b <<>> u1 --ok
        f  = e <<>> u3 --e not ok!!!
        g  =  u2 <<>> e --e not ok!!
        h  = u1 `matrixOr` (u2 <<>> f)  -- f not ok!!
        comp = (e ||| f) === (g ||| h)
        res = if isUniform
            then m
            else step comp

        lkup = 
            do 
                val <- lookup m mem
                return (val, mem3)

        mem3 = insert m res mem
    in
        if rn <= 26
            then fromMaybe (res, if isUniform then mem else mem3) lkup
            else (res, if isUniform then mem else mem2)