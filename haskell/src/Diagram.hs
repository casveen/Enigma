module Diagram(
    drawRotor,
    drawEnigma
) where

import Enigma
import Cartridge
import Plugboard
import Rotor
import Transform
import Cipher
import Language
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Debug.Trace

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

wd :: Double
wd = 3.0
rw :: Double
rw = 10.0


drawRotor :: (Enum e, Ord e) => Rotor e -> Int -> Int -> Diagram B
drawRotor (Rotor transform notches) wireIn wireOut =
    let
        n = letters transform
        --entirety :: [Int]
        entirety        = map (fromEnum . encrypt transform . toEnum) [0..(n-1)]
        --inverseEntirety :: [Int]
        inverseEntirety = map (fromEnum . decrypt transform . toEnum) [0..(n-1)]
        allWires = zip [0..] entirety
        drawWire :: Int -> Int -> Diagram B
        drawWire from to =
            let
                f = wd*(fromIntegral from-(fromIntegral n - 1)/2)
                t = wd*(fromIntegral to-(fromIntegral n - 1)/2)
                outerLeftVertices = map p2 [(-rw-1.0,f),(-rw,f)]
                outerRightVertices = map p2 [(rw+1.0,t),(rw,t)]
                innerVertices = map p2 [
                    (-rw,f),
                    (-0.5*rw, f),
                    ((0.5*rw), t),
                    (rw, t)]
                color
                  | from == wireIn = red
                  | from == wireOut = red
                  | otherwise = gray
            in
                fromVertices outerRightVertices # lc color # lwO 9 <>
                fromVertices outerLeftVertices # lc color # lwO 9 <>
                bspline innerVertices # lc color #lwO 3
                --(fromVertices $ map p2 [(2.0,f),(-2.0,t)]) # lc color
        drawAllWires = mconcat $ map (uncurry drawWire) allWires
    in
        rect (2*rw) (wd*(fromIntegral n)) <> drawAllWires
                                      <> rect (2*rw) (wd*(fromIntegral n)) # fc lightgray
                                      
drawRotor (Reflector transform notches) wireIn wireOut =
    let
        n = letters transform
        entirety :: [Int]
        entirety        = map (fromEnum . encrypt transform . toEnum) [0..(n-1)]
        inverseEntirety :: [Int]
        inverseEntirety = map (fromEnum . decrypt transform . toEnum) [0..(n-1)]

        nonRepeatingEntiretyBuilder ((x,y):xs) =
            if x `elem` (map snd xs)
                then (x,y):nonRepeatingEntiretyBuilder xs
                else nonRepeatingEntiretyBuilder xs
        nonRepeatingEntiretyBuilder [] = []

        allWires =  nonRepeatingEntiretyBuilder $ zip [0..] entirety
        drawWire :: Int -> Int -> Diagram B
        drawWire from to =
            let
                f = wd*(fromIntegral from-(fromIntegral n - 1)/2)
                t = wd*(fromIntegral to-(fromIntegral n - 1)/2)
                outerLeftVertices = map p2 [(-rw-1,t),(-rw,t)]
                outerRightVertices = map p2 [(-rw-1,f),(-rw,f)]

                innerVertices i = map p2 [
                    (-rw,f),
                    (-rw+0.5 + (rw*1.5)*(i/fromIntegral n), f),
                    (-rw+0.5 + (rw*1.5)*(i/fromIntegral n), t),
                    (-rw, t)]

                color
                  | from == wireIn = red
                  | from == wireOut = red
                  | otherwise = gray
            in
                fromVertices outerRightVertices # lc color # lwO 9 <>
                fromVertices outerLeftVertices # lc color # lwO 9 <>
                bspline (innerVertices (fromIntegral from)) # lc color #lwO 3
                --(fromVertices $ map p2 [(2.0,f),(-2.0,t)]) # lc color
        drawAllWires = mconcat $ map (uncurry drawWire) allWires
    in
        rect (2*rw) (wd*(fromIntegral n)) <> drawAllWires
                                      <> rect (2*rw) (wd*(fromIntegral n)) # fc lightgray

drawEnigma :: (Enum e, Ord e) => EnigmaState e -> Int -> Diagram B
drawEnigma enigma@(Enigma plugboard (Cartridge rotors reflector positions)) activeWire =
    let
        rotornum                  = length rotors
        letter                    = toEnum activeWire
        --initWriter                = writer (letter, [letter]) 
        encryptionPathWriter      = evalState (tracedEncryptEnigma letter) enigma
        (cipher, encryptionPat)  = runWriter encryptionPathWriter
        encryptionPath =  Debug.Trace.trace (show $ map fromEnum encryptionPat) $ encryptionPat

        --tupleAsList :: (e,e) -> [e]
        --tupleAsList (x,y) = [x,y] 
        --crimp :: (Enum e, Ord e) => [e] -> [(e,e)] --"folds" in the middle
        crimp xs = 
            let 
                it = take (rotornum+2) (zip xs (reverse xs))
            in
                Debug.Trace.trace (show $ map (\(x,y) -> (fromEnum x, fromEnum y)) it) $ it
        --inOut :: (Enum e, Ord e) => [e] -> ((e,e),[(e,e)],(e,e)) -- TODO to int int, since thats how used in drawrotoer
        inOut xs =
            (let
                (pout:crimped) = crimp xs
                rout           = take rotornum crimped
                refout         = head $ drop rotornum crimped --unsafe
            in
                Debug.Trace.trace (show $ map (\(x,y) -> (fromEnum x, fromEnum y)) rout) $ (pout, rout, refout))
        (plugboardInOut, rotorsInOut, (reflectorIn, reflectorOut)) = inOut $ encryptionPath
    in
        hsep 0.0 [hsep 0.0 $ zipWith (curry
            (\(rotor, (wireIn, wireOut)) -> drawRotor rotor (fromEnum wireIn) (fromEnum wireOut))) rotors rotorsInOut,
            drawRotor reflector (fromEnum reflectorIn) (fromEnum reflectorOut)]