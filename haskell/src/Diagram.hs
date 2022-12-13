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
wd = 2.0
rw :: Double
rw = 10.0

drawPlugboard :: (Enum e, Ord e, Show e) => Plugboard e -> e -> e -> Diagram B
drawPlugboard (Plugboard transform) wireInEnum wireOutEnum =
    let
        wireIn   = fromEnum wireInEnum
        wireOut  = fromEnum wireOutEnum
        n        = letters transform
        ni       = fromIntegral n
        entirety = map (encrypt transform . toEnum) [0..(n-1)]
        allWires = zip (map toEnum [0..]) entirety
        drawAllWires = mconcat $ map (uncurry drawWire) allWires
        drawWire :: (Enum e, Show e) => e -> e -> Diagram B
        drawWire fe te =
            let
                from = mod (fromEnum fe) n 
                to = mod (fromEnum te) n 
                f = wd*(fromIntegral from-(ni - 1)/2)
                t = wd*(fromIntegral to-(ni - 1)/2)
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
                text (show fe) # translateX (-rw-1.0) # translateY (wd*(fromIntegral (fromEnum fe) - (ni-1)/2)) <>
                fromVertices outerRightVertices # lc color # lwO 9 <>
                fromVertices outerLeftVertices # lc color # lwO 9 <>
                bspline innerVertices # lc color # lwO 3 <>
                fromVertices [p2 (-rw,f+wd/2), p2 (rw,f+wd/2)] # lcA (black `withOpacity` 0.3) 
                
    in
        rect (2*rw) (wd*ni) <> drawAllWires
                            <> rect (2*rw) (wd*ni) # fc lightgray

--remember that the enigma offsets the notches as well with the ring setting!
drawRotor :: (Enum e, Ord e, Show e) => Rotor e -> e -> e -> Int -> Diagram B
drawRotor (Rotor transform notches) wireInEnum wireOutEnum offset =
    let
        wireIn   = fromEnum wireInEnum
        wireOut  = fromEnum wireOutEnum
        n        = letters transform
        ni       = fromIntegral n
        entirety = map (encrypt transform . toEnum) [0..(n-1)]
        allWires = zip (map toEnum [0..]) entirety
        drawAllWires = mconcat $ map (uncurry drawWire) allWires
        drawWire :: (Enum e, Show e) => e -> e -> Diagram B
        drawWire fe te =
            let
                from = mod (fromEnum fe-offset) n 
                to = mod (fromEnum te-offset) n 
                f = wd*(fromIntegral from-(ni - 1)/2)
                t = wd*(fromIntegral to-(ni - 1)/2)
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
                text (show fe) # translateX (-rw-1.0) # translateY (wd*(fromIntegral (fromEnum fe) - (ni-1)/2)) <>
                fromVertices outerRightVertices # lc color # lwO 9 <>
                fromVertices outerLeftVertices # lc color # lwO 9 <>
                bspline innerVertices # lc color # lwO 3 <>
                fromVertices [p2 (-rw,f+wd/2), p2 (rw,f+wd/2)] # lcA (black `withOpacity` 0.3) 
                
    in
        rect (2*rw) (wd*ni) <> drawAllWires
                            <> rect (2*rw) (wd*ni) # fc lightgray
                                      
drawRotor (Reflector transform notches) wireInEnum wireOutEnum offset =
    let
        wireIn   = fromEnum wireInEnum 
        wireOut  = fromEnum wireOutEnum
        n        = letters transform
        ni       = fromIntegral n
        entirety :: [Int]
        entirety        = map (fromEnum . encrypt transform . toEnum) [0..(n-1)]

        nonRepeatingEntiretyBuilder ((x,y):xs) =
            if x `elem` (map snd xs)
                then (x,y):nonRepeatingEntiretyBuilder xs
                else nonRepeatingEntiretyBuilder xs
        nonRepeatingEntiretyBuilder [] = []

        allWires =  nonRepeatingEntiretyBuilder $ zip [0..] entirety
        drawWire :: Int -> Int -> Diagram B
        drawWire fe te =
            let
                from = mod (fromEnum fe-offset) n 
                to = mod (fromEnum te-offset) n 
                f = wd*(fromIntegral from-(fromIntegral n - 1)/2)
                t = wd*(fromIntegral to-(fromIntegral n - 1)/2)
                outerLeftVertices = map p2 [(-rw-1,t),(-rw,t)]
                outerRightVertices = map p2 [(-rw-1,f),(-rw,f)]

                innerVertices i = map p2 [
                    (-rw,f),
                    (-rw+0.5 + (rw*2)*(i/fromIntegral n), f),
                    (-rw+0.5 + (rw*2)*(i/fromIntegral n), t),
                    (-rw, t)]

                color
                  | from == wireIn = red
                  | from == wireOut = red
                  | otherwise = gray
            in
                text (show fe) # translateX (-rw-1.0) # translateY (wd*(fromIntegral (fromEnum fe) - (ni-1)/2)) <>
                fromVertices outerRightVertices # lc color # lwO 9 <>
                fromVertices outerLeftVertices # lc color # lwO 9 <>
                bspline (innerVertices (fromIntegral from)) # lc color #lwO 3
                --(fromVertices $ map p2 [(2.0,f),(-2.0,t)]) # lc color
        drawAllWires = mconcat $ map (uncurry drawWire) allWires
    in
        rect (2*rw) (wd*(fromIntegral n)) <> drawAllWires
                                      <> rect (2*rw) (wd*(fromIntegral n)) # fc lightgray

--draw labels between rotors
--


drawEnigma :: (Enum e, Ord e, Show e) => EnigmaState e -> Int -> Diagram B
drawEnigma enigma@(Enigma plugboard (Cartridge rotors reflector positions)) activeWire =
    let
        rotornum                  = length rotors
        letter                    = toEnum activeWire
        encryptionPathWriter      = evalState (tracedEncryptEnigma letter) enigma
        (cipher, encryptionPat)  = runWriter encryptionPathWriter
        encryptionPath =  Debug.Trace.trace (show $ map fromEnum encryptionPat) $ encryptionPat

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
        ((plugboardIn, plugboardOut), rotorsInOut, (reflectorIn, reflectorOut)) = inOut $ encryptionPath
    in
        hsep 0.0 [
            drawPlugboard plugboard plugboardIn plugboardOut,
            hsep 0.0 $ zipWith3 ( curry . curry (\(rotor, ((wireIn, wireOut), pos)) -> drawRotor rotor wireIn wireOut pos)) rotors rotorsInOut positions,
            drawRotor reflector reflectorIn reflectorOut 0] # bgFrame 1.0 orange