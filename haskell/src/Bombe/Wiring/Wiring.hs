module Bombe.Wiring.Wiring (
Wiring(..),
BW,
bundleToWire,
wireToBundleWire    
) where 

type BW                 = (Int, Int)

class Wiring a where
    initialize  :: Int -> a
    connectWire :: a -> BW -> BW -> a
    isConnectedBW :: a -> BW -> BW -> Bool
    isConnected :: a -> Int -> Int -> Bool
    closure :: a -> a
    getLetters :: a -> Int

bundleToWire :: BW -> Int -> Int
bundleToWire (b, w) n = n*b + w

wireToBundleWire :: Integral b => b -> b -> (b, b)
wireToBundleWire i n = (div i n,mod i n)