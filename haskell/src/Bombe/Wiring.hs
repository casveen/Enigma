{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Bombe.Wiring where
import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Control.Monad.Memo

{-
Finding a good solution for representing wiring is hard.
Initially, for storage, a symmetric matrix with booleans and symmetric 26x26 blocks is ideal.
Blas/Lapack does not support Boolean matrices, so we use Int, and ideally Z Mod 2.
However, matrix multiplication does not work like a transition function, as 2->0 mod2, so if several states go to same 
we will get errors. so we use Int. 

Hermetic/Symmetric matrices have no matrix mult instance, so we end up using just Matrix
-}

type WM = Matrix I
data MatrixWiring       = MatrixWiring WM Int deriving (Show, Eq)
data MatrixWiring0      = MatrixWiring0 WM Int deriving (Show)
--while Matrix (Z Mod 2) is ideal, it gives a lot of problems when doing <> between matrices. 
type BW                 = (Int, Int)

bundleToWire :: BW -> Int -> Int
bundleToWire (b, w) n = n*b + w

wireToBundleWire :: Integral b => b -> b -> (b, b)
wireToBundleWire i n = (div i n,mod i n)

upperTriangularOfUpperTriangular :: (Int,Int) -> Int -> (Int,Int)
upperTriangularOfUpperTriangular (r,c) n =
    let
        --first map to upper triangular
        (ur, uc) = (min r c, max r c)
        --find submatrix origin coordinates
        (sr, sc) = (div ur n, div uc n)
        --then find coordinates in submatrix
        (sur, suc) = (mod ur n, mod uc n)
        --map to upper triangular in submatrix
        (usur, usuc) = (min sur suc, max sur suc)
    in
        --map submatrix to matrix
        (n*sr+usur, n*sc+usuc)

class EnigmaWiring a where
    initialize  :: Int -> a
    connectWire :: a -> BW -> BW -> a
    isConnectedBW :: a -> BW -> BW -> Bool
    isConnected :: a -> Int -> Int -> Bool
    closure :: a -> a
    getMatrix :: a -> WM
    getLetters :: a -> Int

instance EnigmaWiring MatrixWiring where
    getMatrix (MatrixWiring m _) = m

    getLetters (MatrixWiring _ n) = n

    isConnectedBW (MatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
            --choose the upper triangle
            --(pi, pj) = (min i j, max i j) --upperTriangularOfUpperTriangular (i,j) n
        in
            m `atIndex` (i,j) > 0

    isConnected (MatrixWiring m n) i j =
        let
            --(pi,pj) = (min i j, max i j) --upperTriangularOfUpperTriangular (i,j) n
        in
             m `atIndex` (i,j) > 0

    connectWire (MatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
            --(pi,pj) = (min i j, max i j)
        in
            MatrixWiring (accum m const [
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
                               --i <= j,
                               i == j || bi == wj && bj == wi]
            matrix =
                assoc
                    (n*n, n*n)
                    0
                    allIndexes
        in
            MatrixWiring matrix n

    closure = startEvalMemo . transitiveClosureMemoized















instance EnigmaWiring MatrixWiring0 where
    getMatrix (MatrixWiring0 m _) = m

    getLetters (MatrixWiring0 _ n) = n

    isConnectedBW (MatrixWiring0 m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
            --choose the upper triangle
            --(pi, pj) = (min i j, max i j) --upperTriangularOfUpperTriangular (i,j) n
        in
            m `atIndex` (i,j) > 0

    isConnected (MatrixWiring0 m n) i j =
        let
            --(pi,pj) = (min i j, max i j) --upperTriangularOfUpperTriangular (i,j) n
        in
             m `atIndex` (i,j) > 0

    connectWire :: MatrixWiring0 -> BW -> BW -> MatrixWiring0
    connectWire (MatrixWiring0 m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
            --(pi,pj) = (min i j, max i j)
        in
            MatrixWiring0 (step (accum m (+) [
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
            MatrixWiring0 matrix n

    closure (MatrixWiring0 m n) = MatrixWiring0 (transitiveClosure m) n


replicateString n = foldl (++) mempty . replicate n

matrixOr m1 m2 = m1 + m2 - m1 * m2 -- XXX problem if not 0,1 matrices!!!

(<<>>) a b = step $ a <> b

transitiveClosure :: WM -> WM
transitiveClosure m =
    let
        rn  = rows m
        cn  = cols m

        isUniform =
            let
                flattened = mconcat (toLists m)
            in
                foldl (\acc x -> acc && (x == head flattened)) True flattened

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
        g  =  u2 <<>> e
        h  = u1 `matrixOr` (u2 <<>> f)
        comp = (e ||| f) === (g ||| h)
        res = if isUniform
            then m
            else step comp
    in
        res


--BAD!! but might be better in the long run?
--might have to store a LOT ie "EVERY" 26^2 x 26^2 quadrant, 
transitiveClosureMemoized :: (MonadMemo MatrixWiring MatrixWiring m) => MatrixWiring -> m MatrixWiring
transitiveClosureMemoized (MatrixWiring m n) = 
    let
        rn  = rows m
        cn  = cols m

        isUniform =
            let
                flattened = mconcat (toLists m)
            in
                foldl (\acc x -> acc && (x == head flattened)) True flattened

        partitionMatrix =
            let
                rs = div rn 2 + mod rn 2 --mod ensures > half
                cs = div cn 2 + mod cn 2 --mod ensures > half
                blocks = toBlocksEvery rs cs m
            in
                ((blocks !! 0 !! 0, blocks !! 0 !! 1),(blocks !! 1 !! 0, blocks !! 1 !! 1))

        ((a,b),(c,d)) = partitionMatrix
    in 
        if isUniform
            then return (MatrixWiring m n)
            else 
                do
                    (MatrixWiring u1 _) <- memo transitiveClosureMemoized (MatrixWiring d n)
                    let u2 = u1 <<>> c
                    (MatrixWiring e _)  <- memo transitiveClosureMemoized (MatrixWiring (a `matrixOr` (b <<>> u2)) n)
                    let
                        u3 = b <<>> u1
                        f  = e <<>> u3
                        g  =  u2 <<>> e
                        h  = u1 `matrixOr` (u2 <<>> f)
                        comp = (e ||| f) === (g ||| h)
                    return (MatrixWiring (step comp) n)

--lexical but >0 equiv to 1
instance Ord MatrixWiring where
    compare :: MatrixWiring -> MatrixWiring -> Ordering
    compare (MatrixWiring m1 _) (MatrixWiring m2 _) =
        let
            flattened1 = mconcat $ toLists (step m1)
            flattened2 = mconcat $ toLists (step m2)
        in
            compare flattened1 flattened2
            
symmetricTransitiveClosure :: WM -> WM
symmetricTransitiveClosure m =
    let
        rn  = rows m
        cn  = cols m

        isUniform =
            let
                flattened = mconcat (toLists m)
            in
                foldl (\acc x -> acc && (x == head flattened)) True flattened

        partitionMatrix =
            let
                rs = div rn 2 + mod rn 2 --mod ensures > half
                cs = div cn 2 + mod cn 2 --mod ensures > half
                blocks = toBlocksEvery rs cs m
            in
                (blocks !! 0 !! 0, blocks !! 0 !! 1, blocks !! 1 !! 1)

        (a, b, d) = partitionMatrix
        c = tr d

        u1 = symmetricTransitiveClosure d --ok
        u2 = u1 <<>> c --ok
        e  = symmetricTransitiveClosure (a `matrixOr` (b <<>> u2)) --ok, lower is dont care, no influence since no <<>>
        u3 = b <<>> u1 --ok
        f  = e <<>> u3 --e not ok!!!
        g  =  u2 <<>> e --e not ok!!
        h  = u1 `matrixOr` (u2 <<>> f)  -- f not ok!!
        comp = (e ||| f) === (g ||| h)
        res = if isUniform
            then m
            else step comp
    in
        res