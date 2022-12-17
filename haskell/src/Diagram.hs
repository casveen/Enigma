{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Diagram(
    drawRotor,
    drawEnigma,
    defaultShape
) where

import Enigma
import Cartridge
import Plugboard
import Rotor
import Cipher
import Control.Monad.Writer.Strict
import Debug.Trace
import Data.Bifunctor()

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
    ( orange,
      bgFrame,
      hsep,
      fcA,
      rect,
      bspline,
      lwO,
      lcA,
      fromVertices,
      translateY,
      translateX,
      (#),
      text,
      p2,
      lightgray,
      darkgray,
      red,
      withOpacity,
      black,
      Bifunctor(bimap),
      Diagram,
      AlphaColour )
import Diagrams.Backend.SVG.CmdLine
import Control.Monad.Reader (Reader, asks, ask, runReader)
import Language (shiftLetter)
import Data.List (zip4)

--wd :: Double
--wd = 2.0
--rw :: Double
--rw = 10.0

data TransformShape = TransformShape {
    transformWidth :: Double,
    transformHeight :: Double,
    transformLineWidth :: Double,
    transformOuterLineWidth :: Double,
    transformDeadZoneWidth :: Double,
    transformBezierSmoothness :: Double, --1 very smooth, 0 very sharp
    transformLetterSize :: Double,
    transformLetterOffset :: Double,
    transformSepLineColor :: AlphaColour Double,
    transformActiveWireColor :: AlphaColour Double,
    transformInactiveWireColor :: AlphaColour Double,
    transformBgColor :: AlphaColour Double
}

defaultShape :: TransformShape
defaultShape = TransformShape 10.0 40.0 3.0 12.0 3.0 0.6 0.5 1.0 (black `withOpacity` 0.1) (red `withOpacity` 1.0) (darkgray `withOpacity` 1.0) (lightgray `withOpacity` 1.0)

drawRotorWire :: (Eq e, Enum e, Show e) => e -> e -> e -> e -> Int -> Int -> Reader TransformShape (Diagram B)
drawRotorWire from to wireIn wireOut n offset = do
    (TransformShape width height lineWidth outerLineWidth deadZoneWidth smoothness letterSize letterOffset sepLineColor activeWireColor inactiveWireColor _) <- ask

    let
        fromShifted = shiftLetter from (-offset) n
        sep = height / fromIntegral n
        f = sep*fromIntegral (mod (fromEnum from-offset) n)
        t = sep*fromIntegral (mod (fromEnum to-offset) n)
        outerLeftVertices = map p2 [
            (-(width + deadZoneWidth)/2,f),
            (-width/2,f)
            ]
        outerRightVertices = map p2 [
            ((width + deadZoneWidth)/2,t),
            (width/2,t)
            ]
        innerVertices = map p2 [
            (-width/2,f),
            (-width/2*smoothness,f),
            (width/2*smoothness,t),
            (width/2,t)
            ]
        color
            | fromShifted == wireIn  = activeWireColor
            | fromShifted == wireOut = activeWireColor
            | otherwise              = inactiveWireColor
    return $
        text (show fromShifted) # translateX (-width/2-letterOffset)
                                # translateY (f - letterSize/2.0)        <>
        fromVertices outerRightVertices # lcA color # lwO outerLineWidth <>
        fromVertices outerLeftVertices # lcA color # lwO outerLineWidth  <>
        bspline innerVertices # lcA color # lwO lineWidth                <>
        fromVertices [p2 (-width/2,f+sep/2), p2 (width/2,f+sep/2)] # lcA sepLineColor # lwO 1.0

drawReflectorWire :: (Eq e, Enum e, Show e) => e -> e -> e -> e -> Int -> Int -> Reader TransformShape (Diagram B)
drawReflectorWire from to wireIn wireOut n offset = do
    (TransformShape width height lineWidth outerLineWidth deadZoneWidth _ letterSize letterOffset sepLineColor activeWireColor inactiveWireColor _) <- ask
    let
        fromShifted = shiftLetter from (-offset) n
        toShifted = shiftLetter to (-offset) n
        sep = height / fromIntegral n
        f = sep*fromIntegral (mod (fromEnum from-offset) n)
        t = sep*fromIntegral (mod (fromEnum to-offset) n)
        outerLeftVertices = map p2 [
            (-(width + deadZoneWidth)/2,f),
            (-width/2,f)
            ]
        outerRightVertices = map p2 [
            (-(width + deadZoneWidth)/2,t),
            (-width/2,t)
            ]
        innerVertices = map p2 [
            (-width/2,f),
            (-width/2+fromIntegral (fromEnum from)/fromIntegral n*width, f),
            (-width/2+fromIntegral (fromEnum from)/fromIntegral n*width, t),
            (-width/2,t)
            ]
        color
            | fromShifted == wireIn  = activeWireColor
            | fromShifted == wireOut = activeWireColor
            | otherwise       = inactiveWireColor
    return $
        text (show fromShifted) # translateX (-width/2-letterOffset)
                                # translateY (f - letterSize/2.0)        <>
        text (show toShifted)   # translateX (-width/2-letterOffset)
                                # translateY (t - letterSize/2.0)        <>
        fromVertices outerRightVertices # lcA color # lwO outerLineWidth <>
        fromVertices outerLeftVertices # lcA color # lwO outerLineWidth  <>
        bspline innerVertices # lcA color # lwO lineWidth                <>
        fromVertices [p2 (-width/2,f+sep/2), p2 (width/2,f+sep/2)] # lcA sepLineColor # lwO 1.0 <>
        fromVertices [p2 (-width/2,t+sep/2), p2 (width/2,t+sep/2)] # lcA sepLineColor # lwO 1.0

drawRotor :: (Enum e, Ord e, Show e) => Rotor e -> e -> e -> Int -> Reader TransformShape (Diagram B)
drawRotor (Rotor transform _) wireInEnum wireOutEnum offset = do
    width   <- asks transformWidth
    height  <- asks transformHeight
    bgColor <- asks transformBgColor

    let n        = letters transform
        entirety = map (encrypt transform . toEnum) [0..(n-1)]
        allWires = zip (map toEnum [0..]) entirety

    allWiresDiagrams <- mapM (\(x,y) -> drawRotorWire x y wireInEnum wireOutEnum n offset) allWires
    allWiresDiagram  <- foldM (\acc x -> return $ acc <> x) mempty allWiresDiagrams

    return $
        rect width height                          <>
        allWiresDiagram   # translateY ((1/fromIntegral n-1)*height/2) <>
        rect width height # fcA bgColor

drawRotor (Reflector transform _) wireInEnum wireOutEnum offset = do
    width   <- asks transformWidth
    height  <- asks transformHeight
    bgColor <- asks transformBgColor

    let n        = letters transform

        nonRepeatingEntiretyBuilder ((x,y):xs) =
            if x `elem` map snd xs
                then (x,y):nonRepeatingEntiretyBuilder xs
                else nonRepeatingEntiretyBuilder xs
        nonRepeatingEntiretyBuilder [] = []

        entirety = map (encrypt transform . toEnum) [0..(n-1)]

        allWires = nonRepeatingEntiretyBuilder $ zip (map toEnum [0..]) entirety

    allWiresDiagrams <- mapM (\(x,y) -> drawReflectorWire x y wireInEnum wireOutEnum n offset) allWires
    allWiresDiagram  <- foldM (\acc x -> return $ acc <> x) mempty allWiresDiagrams

    return $
        rect width height                                              <>
        allWiresDiagram   # translateY ((1/fromIntegral n-1)*height/2) <>
        rect width height # fcA bgColor



drawPlugboard :: (Enum e, Ord e, Show e) => Plugboard e -> e -> e -> Reader TransformShape (Diagram B)
drawPlugboard (Plugboard transform) wireInEnum wireOutEnum = do
    width   <- asks transformWidth
    height  <- asks transformHeight
    bgColor <- asks transformBgColor

    let n        = letters transform
        entirety = map (encrypt transform . toEnum) [0..(n-1)]
        allWires = zip (map toEnum [0..]) entirety

    allWiresDiagrams <- mapM (\(x,y) -> drawRotorWire x y wireInEnum wireOutEnum n 0) allWires
    allWiresDiagram  <- foldM (\acc x -> return $ acc <> x) mempty allWiresDiagrams

    return $
        rect width height                          <>
        allWiresDiagram   # translateY ((1/fromIntegral n-1)*height/2) <>
        rect width height # fcA bgColor




--
--TODO: draw labels between rotors
--

drawEnigma :: (Enum e, Ord e, Show e) => EnigmaState e -> Int -> Reader TransformShape (Diagram B)
drawEnigma enigma@(Enigma plugboard (Cartridge rotors reflector positions)) activeWire = do
    height <- asks transformHeight

    let
        rotornum                  = length rotors
        letter                    = toEnum activeWire
        encryptionPathWriter      = tracedEncrypt enigma letter
        (_, encryptionPath)       = runWriter encryptionPathWriter

        crimp xs =
            let
                it = take (rotornum+2) (zip xs (reverse xs))
            in
                Debug.Trace.trace (show $ map (bimap fromEnum fromEnum) it) it
        inOut xs =
            (let
                (pout:crimped) = crimp xs
                rout           = take rotornum crimped
                refout         = (crimped !! max 0 rotornum) --unsafe
            in
                Debug.Trace.trace (show $ map (bimap fromEnum fromEnum) rout) (pout, rout, refout))
        ((plugboardIn, plugboardOut), rotorsInOut, (reflectorIn, reflectorOut)) = inOut encryptionPath
    --make rotordiagrams
    rotorsL <- mapM (\(rotor,(wireIn, wireOut),pos) -> drawRotor rotor wireIn wireOut pos)
            (zip3 rotors rotorsInOut positions)
    --append names to rotordiagrams
    let rotorsD = zipWith (\r i -> text ("ROTOR"++show i) # translateY (height/1.98) <> r) rotorsL [1..] --want ReaderT shape [Diagram]
    --make plugboard and reflector diagrams
    plugboardD <- drawPlugboard plugboard plugboardIn plugboardOut
    reflectorD <- drawRotor reflector reflectorIn reflectorOut 0
    
    return $ hsep 0.0 [
        text "PLUGBOARD" # translateY (height/1.98) <> plugboardD,
        hsep 0.0 rotorsD,
        text "REFLECTOR" # translateY (height/1.98) <> reflectorD
        ] # bgFrame 1.0 orange