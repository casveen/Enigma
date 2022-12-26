{-# LANGUAGE InstanceSigs #-}

module Bombe.Wiring where
import Numeric.LinearAlgebra
import Debug.Trace
import Language

data BadMatrixWiring    = BadMatrixWiring (Matrix R) Int --bad closure
data MatrixWiring       = MatrixWiring (Matrix R) Int deriving (Show)
data QRMatrixWiring     = QRMatrixWiring (Matrix R) Int
data EagerMatrixWiring  = EagerMatrixWiring (Matrix R) Int
type BW                 = (Int, Int)

bundleToWire :: BW -> Int -> Int
bundleToWire (b, w) n = n*b + w

wireToBundleWire :: Int -> Int -> (Int, Int)
wireToBundleWire i n = (div i n,mod i n)

chain i f a = if i == 0 then a else f (chain (i-1) f a)

powerMat m e
  | e == 1 = m
  | even e = powerMat (m Numeric.LinearAlgebra.<> m) (div e 2)
  | otherwise = powerMat m (div e 2)

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




instance EnigmaWiring QRMatrixWiring where
    --isConnected :: MatrixWiring -> BW -> BW -> Bool
    isConnectedBW (QRMatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            m `atIndex` (i,j) > 0

    --connectWire :: MatrixWiring -> Int -> Int -> MatrixWiring
    connectWire (QRMatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            QRMatrixWiring (accum m (+) [((i,j),1),((j,i),1)]) n

    --initialize :: Int -> MatrixWiring
    initialize n =
        let
            matrix = build
                (n*n,n*n)
                (\i j ->
                    let
                        (bi, wi) = wireToBundleWire (round i) n
                        (bj, wj) = wireToBundleWire (round j) n
                    in
                        if bi == wj && bj == wi then 1.0 else 0.0
                ) ::Matrix R
        in
            QRMatrixWiring matrix n

    closure (QRMatrixWiring m n) = QRMatrixWiring (powerMat m n) n

instance EnigmaWiring MatrixWiring where
    getMatrix (MatrixWiring m _) = m

    getLetters (MatrixWiring _ n) = n

    isConnectedBW :: MatrixWiring -> BW -> BW -> Bool
    isConnectedBW (MatrixWiring m n) ii@(implicantFrom, impliesFrom) jj@(implicantTo, impliesTo) =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n 
            --choose the upper triangle
            (pi, pj) = upperTriangularOfUpperTriangular (i,j) n 
        in
            m `atIndex` (pi,pj) > 0

    isConnected (MatrixWiring m n) i j = 
        let 
            (pi,pj) = upperTriangularOfUpperTriangular (i,j) n
        in
            trace (show (i,j) ++ "->" ++ show (pi,pj)) $ m `atIndex` (pi,pj) > 0

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
            it = bundleToWire (swap f) n
            jt = bundleToWire (swap t) n
            imfL = show (toEnum implicantFrom :: Letter)
            imtL = show (toEnum implicantTo :: Letter)
            imfR = show (toEnum impliesFrom :: Letter)
            imtR = show (toEnum impliesTo :: Letter)
            (pi, pj) = upperTriangularOfUpperTriangular (i,j) n
            --lts = "i:" ++ (show i) ++ "\nit:" ++ (show it) ++ "\nj:" ++ (show j) ++ "\njt:" ++ (show jt) ++ "\n"


            --res = trace (imfL ++ "~" ++ imtL ++ "=>" ++ imfR ++ "~" ++ imtR ++ "\n" ++ lts) $ 
        in
            MatrixWiring (accum m (+) [
                --((pi,pj),1)
                ((i,j),1),
                ((j,i),1)
                ]) n

    --initialize :: Int -> MatrixWiring
    initialize n =
        let
            matrix = build
                (n*n,n*n)
                (\i j ->
                    let
                        (bi, wi) = wireToBundleWire (round i) n
                        (bj, wj) = wireToBundleWire (round j) n
                        (implicantFrom, impliesFrom) = wireToBundleWire (round i) n
                        (implicantTo, impliesTo) = wireToBundleWire (round j) n
                    in
                        if (i==j) || (bi == wj && bj == wi) then 1.0 else 0.0
                        {-if  (implicantFrom==impliesFrom && implicantTo == impliesTo) ||
                            (--(i<j) && 
                            --((implicantFrom == implicantTo && impliesFrom == impliesTo) ||
                            (implicantFrom == impliesFrom && implicantTo == impliesTo)))
                            (implicantFrom == impliesTo && implicantTo == impliesFrom))
                                then 1.0 
                                else 0.0-}
                ) ::Matrix R
        in
            MatrixWiring matrix n

    closure (MatrixWiring m n) = MatrixWiring (chain (n*n) (Numeric.LinearAlgebra.<> m) m) n --(powerMat m (n*n)) n'
    --
    --closure (MatrixWiring m n) = MatrixWiring (chain (n*n) (Numeric.LinearAlgebra.<> m) m) n --(powerMat m (n*n)) n'



instance EnigmaWiring BadMatrixWiring where
    isConnectedBW (BadMatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            m `atIndex` (i,j) > 0

    --connectWire :: MatrixWiring -> Int -> Int -> MatrixWiring
    connectWire (BadMatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            BadMatrixWiring (accum m (+) [((i,j),1),((j,i),1)]) n

    --initialize :: Int -> MatrixWiring
    initialize n =
        let
            matrix = build
                (n*n,n*n)
                (\i j ->
                    let
                        (bi, wi) = wireToBundleWire (round i) n
                        (bj, wj) = wireToBundleWire (round j) n
                    in
                        if bi == wj || bj == wi then 1.0 else 0.0
                ) ::Matrix R
        in
            BadMatrixWiring matrix n

    closure (BadMatrixWiring m n) = BadMatrixWiring (chain n (Numeric.LinearAlgebra.<> m) m) n



instance EnigmaWiring EagerMatrixWiring where
    isConnectedBW (EagerMatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
        in
            m `atIndex` (i,j) > 0

    --connectWire :: MatrixWiring -> Int -> Int -> MatrixWiring
    connectWire (EagerMatrixWiring m n) ii jj =
        let
            i = bundleToWire ii n
            j = bundleToWire jj n
            matrix = accum m (+) [((i,j),1),((j,i),1)]

        in
            EagerMatrixWiring (matrix Numeric.LinearAlgebra.<> matrix) n

    --initialize :: Int -> MatrixWiring
    initialize n =
        let
            matrix = build
                (n*n,n*n)
                (\i j ->
                    let
                        (bi, wi) = wireToBundleWire (round i) n
                        (bj, wj) = wireToBundleWire (round j) n
                    in
                        if bi == wj || bj == wi then 1.0 else 0.0
                ) ::Matrix R
        in
            EagerMatrixWiring matrix n

    closure = id

class EnigmaWiring a where
    initialize  :: Int -> a
    connectWire :: a -> BW -> BW -> a
    isConnectedBW :: a -> BW -> BW -> Bool
    isConnected :: a -> Int -> Int -> Bool
    closure :: a -> a
    getMatrix :: a -> Matrix R
    getLetters :: a -> Int

replicateString n = foldl (++) mempty . replicate n

prettyPrintMatrix    :: Matrix R -> Int -> String
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
        matrixRows matrix = foldl (\acc (r, i) -> acc ++ matrixRow (wireToBundleWire i n) r ++ "\n") "" (zip (toLists matrix) [0..])
    in
        upperRow ++ 
        "\n" ++ 
        matrixRows m



