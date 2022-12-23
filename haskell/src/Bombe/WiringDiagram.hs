{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Bombe.WiringDiagram(
    drawWires,
    drawWiresDefault
) where

import Data.Bifunctor()

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
      (#), lwL )
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.Cairo.CmdLine

import Control.Monad.Reader (Reader, asks, runReader)
import Language

import Bombe.Wiring (EnigmaWiring (getLetters), MatrixWiring (MatrixWiring))
import Numeric.LinearAlgebra (toLists)

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

drawWires :: MatrixWiring -> Reader DiagramShape (Diagram B)
drawWires (MatrixWiring matrix n) = do
    height       <- asks dHeight
    width        <- asks dWidth
    leftAir      <- asks leftAir
    upperAir     <- asks upperAir
    blockShape   <- asks blockShape
    let blockTColor = runReader (asks tColor) blockShape
    let blockFColor = runReader (asks fColor) blockShape
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
                        (rect (blockWidth*fromIntegral n) upperAir                 <> 
                        text (show b) # translateY (upperAir/4.0) -- # translateX (-blockWidth/4.0))  <>
                            ) # translateY (upperAir/2.0)
                            # translateX (blockWidth*fromIntegral n / 2.0)  <>
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
                        (rect leftAir (blockHeight*fromIntegral n)                    <> 
                        text (show b) # translateX (-leftAir/4.0) # translateY (-blockHeight/4.0)) 
                            # translateX (-leftAir/2.0)
                            # translateY (-blockHeight*fromIntegral n / 2.0) <>
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

        drawWiring :: Diagram B
        drawWiring = 
            let
                diagrams = map
                    (map
                        (\e -> rect blockWidth blockHeight # fcA (if e>=1.0 then blockTColor else blockFColor))
                    )
                    listOfRows
            in 
                vsep 0.0 (map (hsep 0.0) diagrams) # translateX (blockWidth/2.0) 
                                                   # translateY (-blockHeight/2.0)
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
        drawWiring # lwL 0.01  -- 
        --drawUpperDiagram <>
        --drawLeftDiagram
    --return mempty

