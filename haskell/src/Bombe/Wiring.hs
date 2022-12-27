{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Bombe.Wiring where
import Prelude hiding ((<>))
import Numeric.LinearAlgebra
import Debug.Trace
--import Language
--import Data.TypeNums
import Numeric.LinearAlgebra.Data
--import GHC.Num.Natural



type ModTwo = Numeric.LinearAlgebra.Data.Mod 2 Z
type UM2 = Matrix Z
type M2 = Herm Z -- hermetic matrix of integers modulo two

data MatrixWiring       = MatrixWiring UM2 Int deriving (Show)

type BW                 = (Int, Int)

bundleToWire :: BW -> Int -> Int
bundleToWire (b, w) n = n*b + w

--wireToBundleWire :: Int -> Int -> (Int, Int)
wireToBundleWire i n = (div i n,mod i n)

{-chain i f a = if i == 0 then a else f (chain (i-    should connect two wires FAILED [1]
powerMat m e
  | e == 1 = m
  | even e = powerMat (m Numeric.LinearAlgebra.<> m) (div e 2)
  | otherwise = powerMat m (div e 2)
-}
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

--transitiveClosureOfUpperTriangular (MatrixWiring m n) = 



instance EnigmaWiring MatrixWiring where
    getMatrix (MatrixWiring m _) = m

    getLetters (MatrixWiring _ n) = n

    isConnectedBW :: MatrixWiring -> BW -> BW -> Bool
    isConnectedBW (MatrixWiring m n) ii@(implicantFrom, impliesFrom) jj@(implicantTo, impliesTo) =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
            --choose the upper triangle
            (pi, pj) = (i,j) --upperTriangularOfUpperTriangular (i,j) n
        in
            ( m) `atIndex` (pi,pj) > 0

    isConnected (MatrixWiring m n) i j =
        let
            (pi,pj) = (i,j)--upperTriangularOfUpperTriangular (i,j) n
        in
            trace (show (i,j) ++ "->" ++ show (pi,pj)) $ (m) `atIndex` (pi,pj) > 0

    --connectWire :: MatrixWiring -> Int -> Int -> MatrixWiring
    {-connectWire (MatrixWiring m n) ii jj =
        let
            swap (x,y) = (y,x)
            i = bundleToWire (swap ii) n
            j = bundleToWire (swap jj) n
        in
            MatrixWiring (accum m (+) [((i,j),1),((j,i),1)]) n-}
    connectWire (MatrixWiring m n) f@(implicantFrom, implicantTo) t@(impliesFrom, impliesTo) =
        let
            swap (x,y) = (y,x)
            i = bundleToWire f n
            j = bundleToWire t n
            um = m
            --lts = "i:" ++ (show i) ++ "\nit:" ++ (show it) ++ "\nj:" ++ (show j) ++ "\njt:" ++ (show jt) ++ "\n"
-- >>> let hilb n = build (n,n) (\i j -> 1/(i+j+1)) :: Matrix Double

            --res = trace (imfL ++ "~" ++ imtL ++ "=>" ++ imfR ++ "~" ++ imtR ++ "\n" ++ lts) $ 
        in
            MatrixWiring ( step (accum um (+) [
                --((pi,pj),1)
                ((i,j),1),
                ((j,i),1)
                ])) n

    --initialize :: Int -> MatrixWiring
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
            MatrixWiring (matrix) n

    closure (MatrixWiring m n) = 
        let
            um =  m  
            --chained = chain (n*n) (Numeric.LinearAlgebra.<> um) um
            res = transitiveClosure um
        in 
            MatrixWiring ( res) n --(powerMat m (n*n)) n'
    --
    --closure (MatrixWiring m n) = MatrixWiring (chain (n*n) (Numeric.LinearAlgebra.<> m) m) n --(powerMat m (n*n)) n'



class EnigmaWiring a where
    initialize  :: Int -> a
    connectWire :: a -> BW -> BW -> a
    isConnectedBW :: a -> BW -> BW -> Bool
    isConnected :: a -> Int -> Int -> Bool
    closure :: a -> a
    getMatrix :: a -> UM2
    getLetters :: a -> Int

replicateString n = foldl (++) mempty . replicate n

prettyPrintMatrix    :: M2 -> Int -> String
prettyPrintMatrix m n =
    let
        upperRow =  "    " ++
                    foldl
                        (\acc l -> acc ++ replicateString (n-1) (l++" ") ++ l ++ "|" )
                        ""
                        (map show [0..(n-1)]) ++
                    "\n    " ++
                    replicateString
                        n
                        (foldl
                            (\acc l -> acc ++ l ++ " " )
                            ""
                            (map show [0..(n-2)])
                            ++
                            show (n-1) ++ "|"
                        )
        matrixRow (b,w) row = show b ++ " " ++
                              show w ++ " " ++
                              foldl (\acc x ->acc ++ if x>0 then "O " else "  ") "" row
        matrixRows matrix = foldl (\acc (r, i) -> acc ++ matrixRow (wireToBundleWire i n) r ++ "\n") "" (zip (toLists (unSym matrix)) [0..])
    in
        upperRow ++
        "\n" ++
        matrixRows m



matrixOr m1 m2 = m1 + m2 - m1 * m2

divCeil x y = div x y

--mod2 transition, remember that we cannot use <> since 2 -> 0
(<<>>) a b = step $ a <> b

transitiveClosure :: UM2 -> UM2
transitiveClosure m = 
    let
        --um = unSym m
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

        u1 = transitiveClosure d --1x1
        u2 = u1 <<>> c -- 1x1 <> 1x2 -> 1x2
        --b <> u2 -> 2x1 <> 1x2 -> 2x2
        e = transitiveClosure (a `matrixOr` (b <<>> u2)) --2x2
        u3 = b <<>> u1 --2x1 <> 1x1 -> 2x1
        f = e <<>> u3 -- 2x2 <> 2x1 -> 2x1
        g =  u2 <<>> e --1x2 <> 2x2 -> 1x2
        h = u1 `matrixOr` (u2 <<>> f) --1x1 or 1x2 <> 2x1 -> 1x1
        comp = (e ||| f) === (g ||| h)
        res = if isUniform 
            then m 
            else step comp--fromBlocks [[e,f],[g,h]] -- [[2x2, 2x1], [1x2, 1x1]] ok...
    in 
        res


symmetricTransitiveClosure :: M2 -> M2
symmetricTransitiveClosure m = 
    let
        um = unSym m
        rn  = rows um 
        cn  = cols um
        isUniform = 
            let 
                flattened = mconcat (toLists um)
            in 
                foldl (\acc x -> acc && (x == head flattened)) True flattened

        partitionMatrix = 
            let 
                blocks = toBlocksEvery (div rn 2) (div cn 2) um
            in 
                ((blocks !! 0 !! 0, blocks !! 0 !! 1),(blocks !! 1 !! 0, blocks !! 1 !! 1))
        ((a,b),(c,d)) = partitionMatrix
        u1 = unSym $ symmetricTransitiveClosure (trustSym d) -- d is symmetric, so OK!
        u2 = u1 <> tr b
        e = unSym (symmetricTransitiveClosure $ trustSym (a + (b <> u2))) -- also symmetric, so ok!
        u3 = b <> u1
        f = e <> u3 
        g =  u2 <> e
        h = u1 + (u2 <> f)

        res = if isUniform 
            then m 
            else trustSym $ fromBlocks [[e,f],[g,h]]
    in 
        trace ("The transitive closure of " ++ (show m) ++ "is " ++ (show res)) $ res