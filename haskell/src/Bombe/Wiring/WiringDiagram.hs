{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-type-defaults -Wno-name-shadowing -Wno-incomplete-uni-patterns  #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Bombe.Wiring.WiringDiagram(
    drawWires,
    drawWiresDefault,
    DiagramShape(..),
    BlockShape(..),
    drawWiresSymmetric
) where

import Data.Bifunctor()
import Diagrams.Prelude
    ( AlphaColour,
      Diagram,
      black,
      white,
      withOpacity,
      green,
      lightgray,
      fcA,
      hsep,
      vsep,
      rect,
      text,
      translateX,
      translateY,
      (#), lwL, gray )
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo.CmdLine ( B )
import Control.Monad.Reader (Reader, asks, runReader)
--import Language ( Letter(Z, A) )
import Diagrams (fromVertices, p2)
import Numeric.LinearAlgebra.Data (atIndex)
import Bombe.Wiring.MatrixWiring.MatrixWiring (MatrixWiring(..))
import Bombe.Wiring.Wiring (Wiring(..))

-------------------------------------
--             SHAPES              --
-------------------------------------

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

-------------------------------------
--     DIAGRAMS - DEFAULTSHAPES    -- 
-------------------------------------
drawWiresDefault ::  MatrixWiring m => m -> Diagram B
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

drawWiresSymmetricDefault :: MatrixWiring m => m -> Diagram B
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

-------------------------------------
--              DIAGRAMS           -- 
-------------------------------------
drawWiresSymmetric :: MatrixWiring m => m -> Reader DiagramShape (Diagram B)
drawWiresSymmetric m = do
    --let mat = getMatrix m
    let n = getLetters m
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
        -- matrix = mat --unSym mat
        letters           = take n [0 .. ]
        -- lettersAndLetters = [(b,w) | w <- letters, b <- letters ]
        blockHeight = (height - upperAir) / fromIntegral (n*n)
        blockWidth  = (width - leftAir) / fromIntegral (n*n)

        drawUpperDiagram :: Diagram B
        drawUpperDiagram  =
            hsep 0.0 (
                map
                    (\b ->
                        (rect (blockWidth*ni) upperAir                 <>
                        text (show b) # translateY (upperAir/4.0)
                            ) # translateY (upperAir/2.0)
                            # translateX (blockWidth*ni / 2.0)  <>
                        hsep 0.0 (
                            map
                                (\w ->
                                    rect blockWidth (upperAir/2.0) # translateY (upperAir/4.0)    <>
                                    text (show w) # translateY (upperAir/4.0)
                                )
                                letters
                        ) # translateX (blockWidth/2.0)
                    )
                    letters
            )  <>
            rect (width-leftAir) upperAir # fcA diagramColor # translateX ((width-leftAir)/2.0) # translateY (upperAir/2.0)

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

        indexedEntries = mconcat $ map (\r -> map (\c -> (r, c, isConnected m r c)) [0..(n*n-1)]) [0..(n*n-1)]

        {-drawWiring :: Diagram B
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
                                                       # translateY (-(height-upperAir)/2.0)-}
        drawWiringWithoutSymmetry :: Diagram B
        drawWiringWithoutSymmetry =
            let
                filteredDiagrams =
                    filter (\(i,j, e) -> i<=j && mod i n <= mod j n && e) indexedEntries
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
                            mod i n > mod j n
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
                fromVertices [p2 (fromIntegral i*blockWidth,0.0), p2 (fromIntegral i*blockWidth, -(height-upperAir))] | i <- [0..(n*n-1)]
            ]) # fcA (black `withOpacity` 1.0) # lwL 0.01 <>
            (mconcat [
                fromVertices [p2 (0.0,(-fromIntegral j)*(ni*blockHeight)), p2 (width-leftAir,(-fromIntegral j)*(ni*blockHeight))] | j <- [0..(n-1)]
            ] <>
            mconcat [
                fromVertices [p2 (fromIntegral i*(ni*blockWidth),0.0), p2 (fromIntegral i*(ni*blockWidth), -(height-upperAir))] | i <- [0..(n-1)]
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











































drawWires :: (MatrixWiring m) => m -> Reader DiagramShape (Diagram B)
drawWires m = do
    let mat = getMatrix m
    let n = getLetters m
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
        matrix = mat --unSym mat
        letters           = take n [0 .. ]
        --lettersAndLetters = [(b,w) | w <- letters, b <- letters ]
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

        --indexedEntries = mconcat $ map (\r -> map (\c -> (r, c, abs (matrix `atIndex` (r,c) ) >0) ) [0..(n*n-1)]) [0..(n*n-1)]
        indexedEntries = mconcat $ map (\r -> map (\c -> (r, c, isConnected m r c) ) [0..(n*n-1)]) [0..(n*n-1)]

        drawWiring :: Diagram B
        drawWiring =
            let
                filteredDiagrams =
                    filter (\(_, _, e) -> e) indexedEntries
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
        {-drawWiringWithoutSymmetry :: Diagram B
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
                                                       # translateY (-(height-upperAir)/2.0)-}

        drawGrid :: Diagram B
        drawGrid =
            (mconcat [
                fromVertices [p2 (0.0,(-fromIntegral j)*blockHeight), p2 (width-leftAir,(-fromIntegral j)*blockHeight)] | j <- [0..(n*n-1)]
            ] <>
            mconcat [
                fromVertices [p2 (fromIntegral i*blockWidth,0.0), p2 (fromIntegral i*blockWidth, -(height-upperAir))] | i <- [0..(n*n-1)]
            ]) # fcA (black `withOpacity` 1.0) # lwL 0.01 <>
            (mconcat [
                fromVertices [p2 (0.0,(-fromIntegral j)*(ni*blockHeight)), p2 (width-leftAir,(-fromIntegral j)*(ni*blockHeight))] | j <- [0..(n-1)]
            ] <>
            mconcat [
                fromVertices [p2 (fromIntegral i*(ni*blockWidth),0.0), p2 (fromIntegral i*(ni*blockWidth), -(height-upperAir))] | i <- [0..(n-1)]
            ]) # fcA (black `withOpacity` 1.0) # lwL 0.05

    return $
        drawLeftDiagram # lwL 0.01 <>
        drawUpperDiagram # lwL 0.01  <>
        drawGrid <>
        drawWiring # lwL 0.01