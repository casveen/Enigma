{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module Bombe.Wiring.MatrixWiring.MatrixWiring where

import Numeric.LinearAlgebra(Matrix(..), I, Element, Container, Vector, toLists, step)
import Data.Map (Map)
import Bombe.Wiring.Wiring (BW, Wiring)

{-
Finding a good solution for representing wiring is hard.
Initially, for storage, a symmetric matrix with booleans and symmetric 26x26 blocks is ideal.
Blas/Lapack does not support Boolean matrices, so we use Int, and ideally Z Mod 2.
However, matrix multiplication does not work like a transition function, as 2->0 mod2, so if several states go to same 
we will get errors. so we use Int. 

Hermetic/Symmetric matrices have no matrix mult instance, so we end up using just Matrix
-}

type WM = Matrix I



--while Matrix (Z Mod 2) is ideal, it gives a lot of problems when doing <> between matrices. 


--instance Container WM where



--not used! compressed matrixwiring does away with this!
{-upperTriangularOfUpperTriangular :: (Int,Int) -> Int -> (Int,Int)
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
        (n*sr+usur, n*sc+usuc)-}

type Mem = Map WM WM

class Wiring a => MatrixWiring a where
    getMatrix :: a -> WM
    memoizedClosure :: (a, Mem) -> (a, Mem)

--replicateString n = Prelude.foldl (++) mempty . replicate n



--lexical but >0 equiv to 1
instance (Element a, Num a, Ord a, Container Vector a) => Ord (Matrix a) where
    compare :: Matrix a -> Matrix a -> Ordering
    compare m1 m2 =
        let
            flattened1 = mconcat $ toLists (step m1)
            flattened2 = mconcat $ toLists (step m2)
        in
            compare flattened1 flattened2