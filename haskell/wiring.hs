import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Prelude hiding ((<>))

configGenerator :: Int -> (Int, Int) -> I
configGenerator k (i,j) = if ((j-div i k)-mod (k*i) (k*k)  == 0) || (i==j) then 1 else 0

configurationMatrix :: Int -> Matrix I
configurationMatrix n = ((n*n) >< (n*n)) $  map (\i -> configGenerator n (mod i (n*n), div i (n*n))) [0..]
b :: Matrix I
b = configurationMatrix 2
c :: Matrix I
c = configurationMatrix 3
d :: Matrix I
d = configurationMatrix 4
--stack ghci --package matrix

transformGenerator k (i,j) rr cc t
  | div i k == rr && div j k == cc =
   t k (mod i k, mod j k)
  | div i k == cc && div j k == rr =
        t k (mod j k, mod i k)
  | i == j = 1
  | otherwise = 0


--matrixFromTransform :: (Foreign.Storable.Storable a, Num a) => (Int -> (Int, Int) -> a) -> Int -> Int -> Int -> Matrix a
matrixFromTransform t rr cc n = ((n*n) >< (n*n)) $ map (\i -> transformGenerator n (mod i (n*n), div i (n*n)) rr cc t) [0..]

tt k (i,j) = [0,0,1,0,0,0,0,1,1,0,0,0,0,1,0,0] !! (i*4+j)

mt :: Matrix I
mt = matrixFromTransform tt 0 3 4



closure = ((d <> mt) + (mt <> d)) <> ((d <> mt) + (mt <> d))
cl = closure <> closure


prt mat = putStr $
    foldr1 (\r s -> r ++ "\n" ++ s) $
    map (foldr1 (++)) $
    map (\lst -> map (\x -> if x==0 then "  " else "X ") lst) (toLists mat)

main = do
    prt cl