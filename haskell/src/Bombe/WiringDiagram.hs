{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Bombe.WiringDiagram(
    drawWires,
    drawWiresDefault,
    DiagramShape(..),
    BlockShape(..),
    drawWiresSymmetric
) where

import Data.Bifunctor()

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
import Diagrams.Prelude
    ( AlphaColour,
      Diagram,
      black,
      white,
      withOpacity,
      green,
      lightgray,
      red,
      fcA,
      hsep,
      vsep,
      rect,
      text,
      translateX,
      translateY,
      showOrigin,
      (#), lwL, gray )
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo.CmdLine

import Control.Monad.Reader (Reader, asks, runReader)
import Language

import Bombe.Wiring (EnigmaWiring (getLetters, isConnected, getMatrix), MatrixWiring (MatrixWiring))
import Numeric.LinearAlgebra (toLists, qr)
import Diagrams (p2)
import Diagrams (fromVertices)
import Numeric.LinearAlgebra.Data (atIndex)

--wd :: Double
--wd = 2.0
--rw :: Double
--rw = 10.0
data BlockShape = BlockShape {
    tColor :: AlphaColour Double,
    fColor :: AlphaColour Double,
    drawF  :: Bool
}

data DiagramShape = DiagramShape {
    dHeight    :: Double,
    dWidth     :: Double,
    leftAir    :: Double,
    upperAir   :: Double,
    blockShape :: BlockShape,
    diagramColor :: AlphaColour Double
}

drawWiresDefault :: MatrixWiring -> Diagram B
drawWiresDefault wiring =
    runReader (drawWires wiring)
    (DiagramShape
        100.0
        100.0
        6.0
        6.0
        (BlockShape
            (green `withOpacity` 1.0)
            (white `withOpacity` 1.0)
            True)
        (lightgray `withOpacity` 1.0)
    )

drawWiresSymmetricDefault :: MatrixWiring -> Diagram B
drawWiresSymmetricDefault wiring =
    runReader (drawWiresSymmetric wiring)
    (DiagramShape
        100.0
        100.0
        6.0
        6.0
        (BlockShape
            (green `withOpacity` 1.0)
            (white `withOpacity` 1.0)
            True)
        (lightgray `withOpacity` 1.0)
    )

drawWiresSymmetric :: MatrixWiring -> Reader DiagramShape (Diagram B)
drawWiresSymmetric m@(MatrixWiring matrix n) = do
    height       <- asks dHeight
    width        <- asks dWidth
    leftAir      <- asks leftAir
    upperAir     <- asks upperAir
    blockShape   <- asks blockShape
    let blockTColor = runReader (asks tColor) blockShape
    let blockFColor = runReader (asks fColor) blockShape
    let ni = fromIntegral n
    diagramColor <- asks diagramColor
    let
        letters           = take n [A .. Z]
        lettersAndLetters = [(b,w) | w <- letters, b <- letters ]
        blockHeight = (height - upperAir) / fromIntegral (n*n)
        blockWidth  = (width - leftAir) / fromIntegral (n*n)

        drawUpperDiagram :: Diagram B
        drawUpperDiagram  =
            hsep 0.0 (
                map
                    (\b ->
                        (rect (blockWidth*ni) upperAir                 <>
                        text (show b) # translateY (upperAir/4.0) -- # translateX (-blockWidth/4.0))  <>
                            ) # translateY (upperAir/2.0)
                            # translateX (blockWidth*ni / 2.0)  <>
                        hsep 0.0 (
                            map
                                (\w ->
                                    rect blockWidth (upperAir/2.0) # translateY (upperAir/4.0)    <>
                                    text (show w) # translateY (upperAir/4.0) -- # translateX (-blockWidth/4.0)
                                )
                                letters
                        ) # translateX (blockWidth/2.0)
                    )
                    letters
            )  <>
            rect (width-leftAir) upperAir # fcA diagramColor # translateX ((width-leftAir)/2.0) # translateY (upperAir/2.0)

            {-drawUpperDiagram  =
            hsep 0.0 (
                map
                    (\(b,w) ->
                        rect blockWidth upperAir <>
                        text (show b ++ " " ++ show w) # translateY (-blockHeight/2.0)
                    )
                    lettersAndLetters
            )  # translateX (blockWidth/2.0) # translateY (upperAir/2.0) <>
            rect (width-leftAir) upperAir # fcA diagramColor              # translateX ((width-leftAir)/2.0) # translateY (upperAir/2.0) <>
            rect (width-leftAir) upperAir # fcA (black `withOpacity` 1.0) # translateX ((width-leftAir)/2.0) # translateY (upperAir/2.0)-}


        drawLeftDiagram :: Diagram B
        drawLeftDiagram  =
            vsep 0.0 (
                map
                    (\b ->
                        (rect leftAir (blockHeight*ni)                    <>
                        text (show b) # translateX (-leftAir/4.0) # translateY (-blockHeight/4.0))
                            # translateX (-leftAir/2.0)
                            # translateY (-blockHeight*ni / 2.0) <>
                        vsep 0.0 (
                            map
                                (\w ->
                                    rect (leftAir/2.0) blockHeight # translateX (-leftAir/4.0) <>
                                    text (show w) # translateX (-leftAir/4.0) # translateY (-blockHeight/4.0)
                                )
                                letters
                        ) # translateY (-blockHeight/2.0)
                    )
                    letters
            )  <>
            rect leftAir (height-upperAir) # fcA diagramColor # translateY (-(height-upperAir)/2.0) # translateX (-leftAir/2.0)

        listOfRows = toLists matrix
        indexedEntries = mconcat $ map (\r -> map (\c -> (r, c, isConnected m r c)) [0..(n*n-1)]) [0..(n*n-1)]
        --        (zip listOfRows [0..])
        --indexedEntries = concatMap
        --        (\(r,ri) -> zipWith (\e ci -> (ri, ci, e)) r [0..])
        --        (zip listOfRows [0..])

        drawWiring :: Diagram B
        drawWiring =
            let
                filteredDiagrams =
                    filter (\(_, _, e) -> e) indexedEntries
                --diagrams :: [Diagram B]
                diagrams = mconcat $
                    map
                        (\(ri, ci, _) ->
                            rect blockWidth blockHeight # fcA blockTColor
                                                        # translateX (blockWidth*fromIntegral ci)
                                                        # translateY (-blockHeight*fromIntegral ri))
                    filteredDiagrams
            in
                diagrams # translateX (blockWidth/2.0)
                         # translateY (-blockHeight/2.0) <>
                rect (width-leftAir) (height-upperAir) # fcA blockFColor
                                                       # translateX ((width-leftAir)/2.0)
                                                       # translateY (-(height-upperAir)/2.0)
        drawWiringWithoutSymmetry :: Diagram B
        drawWiringWithoutSymmetry =
            let
                filteredDiagrams =
                    filter (\(i,j, e) -> i<=j && (mod i n <= mod j n ) && e) indexedEntries
                --diagrams :: [Diagram B]
                diagrams = mconcat $
                    map
                        (\(ri, ci, _) ->
                            rect blockWidth blockHeight # fcA blockTColor
                                                        # translateX (blockWidth*fromIntegral ci)
                                                        # translateY (-blockHeight*fromIntegral ri))
                    filteredDiagrams
                greyedOutDiagrams = 
                    filter 
                        (\(i, j, _) -> 
                            i>j || 
                            (mod i n > mod j n )
                        ) 
                        indexedEntries

                greyedOut = mconcat $
                    map
                        (\(ri, ci, _) ->
                            rect blockWidth blockHeight # fcA (gray `withOpacity` 0.6)
                                                        # translateX (blockWidth*fromIntegral ci)
                                                        # translateY (-blockHeight*fromIntegral ri))
                        greyedOutDiagrams
            in
                diagrams                               # translateX (blockWidth/2.0)
                                                       # translateY (-blockHeight/2.0) <>
                greyedOut                              # translateX (blockWidth/2.0)
                                                       # translateY (-blockHeight/2.0) <>
                rect (width-leftAir) (height-upperAir) # fcA blockFColor
                                                       # translateX ((width-leftAir)/2.0)
                                                       # translateY (-(height-upperAir)/2.0)

        drawGrid :: Diagram B
        drawGrid =
            (mconcat [
                fromVertices [p2 (0.0,(-fromIntegral j)*blockHeight), p2 (width-leftAir,(-fromIntegral j)*blockHeight)] | j <- [0..(n*n-1)]
            ] <>
            mconcat [
                fromVertices [p2 ((fromIntegral i)*blockWidth,0.0), p2 ((fromIntegral i)*blockWidth, -(height-upperAir))] | i <- [0..(n*n-1)]
            ]) # fcA (black `withOpacity` 1.0) # lwL 0.01 <>
            (mconcat [
                fromVertices [p2 (0.0,(-fromIntegral j)*(ni*blockHeight)), p2 (width-leftAir,(-fromIntegral j)*(ni*blockHeight))] | j <- [0..(n-1)]
            ] <>
            mconcat [
                fromVertices [p2 ((fromIntegral i)*(ni*blockWidth),0.0), p2 ((fromIntegral i)*(ni*blockWidth), -(height-upperAir))] | i <- [0..(n-1)]
            ]) # fcA (black `withOpacity` 1.0) # lwL 0.05

        
                {-foldl 
                    (\dia rowDia -> 
                        dia # translateY (-height+upperAir) <>
                        rowDia
                    ) 
                    mempty-}




    return $
        drawLeftDiagram # lwL 0.01 <> -- # translateX (width/2.0) 
                         -- # translateY (upperAir/2.0)) <> -- # translateY (height/2.0) <>
        drawUpperDiagram # lwL 0.01  <>
        drawGrid <>
        --drawWiring # lwL 0.01 <>
        drawWiringWithoutSymmetry # lwL 0.01











































drawWires :: MatrixWiring -> Reader DiagramShape (Diagram B)
drawWires m@(MatrixWiring matrix n) = do
    height       <- asks dHeight
    width        <- asks dWidth
    leftAir      <- asks leftAir
    upperAir     <- asks upperAir
    blockShape   <- asks blockShape
    let blockTColor = runReader (asks tColor) blockShape
    let blockFColor = runReader (asks fColor) blockShape
    let ni = fromIntegral n
    diagramColor <- asks diagramColor
    let
        letters           = take n [A .. Z]
        lettersAndLetters = [(b,w) | w <- letters, b <- letters ]
        blockHeight = (height - upperAir) / fromIntegral (n*n)
        blockWidth  = (width - leftAir) / fromIntegral (n*n)

        drawUpperDiagram :: Diagram B
        drawUpperDiagram  =
            hsep 0.0 (
                map
                    (\b ->
                        (rect (blockWidth*ni) upperAir                 <>
                        text (show b) # translateY (upperAir/4.0) -- # translateX (-blockWidth/4.0))  <>
                            ) # translateY (upperAir/2.0)
                            # translateX (blockWidth*ni / 2.0)  <>
                        hsep 0.0 (
                            map
                                (\w ->
                                    rect blockWidth (upperAir/2.0) # translateY (upperAir/4.0)    <>
                                    text (show w) # translateY (upperAir/4.0) -- # translateX (-blockWidth/4.0)
                                )
                                letters
                        ) # translateX (blockWidth/2.0)
                    )
                    letters
            )  <>
            rect (width-leftAir) upperAir # fcA diagramColor # translateX ((width-leftAir)/2.0) # translateY (upperAir/2.0)

            {-drawUpperDiagram  =
            hsep 0.0 (
                map
                    (\(b,w) ->
                        rect blockWidth upperAir <>
                        text (show b ++ " " ++ show w) # translateY (-blockHeight/2.0)
                    )
                    lettersAndLetters
            )  # translateX (blockWidth/2.0) # translateY (upperAir/2.0) <>
            rect (width-leftAir) upperAir # fcA diagramColor              # translateX ((width-leftAir)/2.0) # translateY (upperAir/2.0) <>
            rect (width-leftAir) upperAir # fcA (black `withOpacity` 1.0) # translateX ((width-leftAir)/2.0) # translateY (upperAir/2.0)-}


        drawLeftDiagram :: Diagram B
        drawLeftDiagram  =
            vsep 0.0 (
                map
                    (\b ->
                        (rect leftAir (blockHeight*ni)                    <>
                        text (show b) # translateX (-leftAir/4.0) # translateY (-blockHeight/4.0))
                            # translateX (-leftAir/2.0)
                            # translateY (-blockHeight*ni / 2.0) <>
                        vsep 0.0 (
                            map
                                (\w ->
                                    rect (leftAir/2.0) blockHeight # translateX (-leftAir/4.0) <>
                                    text (show w) # translateX (-leftAir/4.0) # translateY (-blockHeight/4.0)
                                )
                                letters
                        ) # translateY (-blockHeight/2.0)
                    )
                    letters
            )  <>
            rect leftAir (height-upperAir) # fcA diagramColor # translateY (-(height-upperAir)/2.0) # translateX (-leftAir/2.0)

        listOfRows = toLists matrix
        indexedEntries = mconcat $ map (\r -> map (\c -> (r, c, abs ((getMatrix m) `atIndex` (r,c) ) >=0.001) ) [0..(n*n-1)]) [0..(n*n-1)]
        --        (zip listOfRows [0..])
        --indexedEntries = concatMap
        --        (\(r,ri) -> zipWith (\e ci -> (ri, ci, e)) r [0..])
        --        (zip listOfRows [0..])

        drawWiring :: Diagram B
        drawWiring =
            let
                filteredDiagrams =
                    filter (\(_, _, e) -> e) indexedEntries
                --diagrams :: [Diagram B]
                diagrams = mconcat $
                    map
                        (\(ri, ci, _) ->
                            rect blockWidth blockHeight # fcA blockTColor
                                                        # translateX (blockWidth*fromIntegral ci)
                                                        # translateY (-blockHeight*fromIntegral ri))
                    filteredDiagrams
            in
                diagrams # translateX (blockWidth/2.0)
                         # translateY (-blockHeight/2.0) <>
                rect (width-leftAir) (height-upperAir) # fcA blockFColor
                                                       # translateX ((width-leftAir)/2.0)
                                                       # translateY (-(height-upperAir)/2.0)
        drawWiringWithoutSymmetry :: Diagram B
        drawWiringWithoutSymmetry =
            let
                filteredDiagrams =
                    filter (\(i,j, e) -> i<=j && (mod i n <= mod j n ) && e) indexedEntries
                --diagrams :: [Diagram B]
                diagrams = mconcat $
                    map
                        (\(ri, ci, _) ->
                            rect blockWidth blockHeight # fcA blockTColor
                                                        # translateX (blockWidth*fromIntegral ci)
                                                        # translateY (-blockHeight*fromIntegral ri))
                    filteredDiagrams
                greyedOutDiagrams = 
                    filter 
                        (\(i, j, _) -> 
                            i>j || 
                            (mod i n > mod j n )
                        ) 
                        indexedEntries

                greyedOut = mconcat $
                    map
                        (\(ri, ci, _) ->
                            rect blockWidth blockHeight # fcA (gray `withOpacity` 0.6)
                                                        # translateX (blockWidth*fromIntegral ci)
                                                        # translateY (-blockHeight*fromIntegral ri))
                        greyedOutDiagrams
            in
                diagrams                               # translateX (blockWidth/2.0)
                                                       # translateY (-blockHeight/2.0) <>
                greyedOut                              # translateX (blockWidth/2.0)
                                                       # translateY (-blockHeight/2.0) <>
                rect (width-leftAir) (height-upperAir) # fcA blockFColor
                                                       # translateX ((width-leftAir)/2.0)
                                                       # translateY (-(height-upperAir)/2.0)

        drawGrid :: Diagram B
        drawGrid =
            (mconcat [
                fromVertices [p2 (0.0,(-fromIntegral j)*blockHeight), p2 (width-leftAir,(-fromIntegral j)*blockHeight)] | j <- [0..(n*n-1)]
            ] <>
            mconcat [
                fromVertices [p2 ((fromIntegral i)*blockWidth,0.0), p2 ((fromIntegral i)*blockWidth, -(height-upperAir))] | i <- [0..(n*n-1)]
            ]) # fcA (black `withOpacity` 1.0) # lwL 0.01 <>
            (mconcat [
                fromVertices [p2 (0.0,(-fromIntegral j)*(ni*blockHeight)), p2 (width-leftAir,(-fromIntegral j)*(ni*blockHeight))] | j <- [0..(n-1)]
            ] <>
            mconcat [
                fromVertices [p2 ((fromIntegral i)*(ni*blockWidth),0.0), p2 ((fromIntegral i)*(ni*blockWidth), -(height-upperAir))] | i <- [0..(n-1)]
            ]) # fcA (black `withOpacity` 1.0) # lwL 0.05

        
                {-foldl 
                    (\dia rowDia -> 
                        dia # translateY (-height+upperAir) <>
                        rowDia
                    ) 
                    mempty-}




    return $
        drawLeftDiagram # lwL 0.01 <> -- # translateX (width/2.0) 
                         -- # translateY (upperAir/2.0)) <> -- # translateY (height/2.0) <>
        drawUpperDiagram # lwL 0.01  <>
        drawGrid <>
        --drawWiring # lwL 0.01 <>
        drawWiring # lwL 0.01