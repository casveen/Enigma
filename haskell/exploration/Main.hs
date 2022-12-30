module Main(main) where

import Parts ( simple4 )
import Bombe.Wiring.Wiring ( Wiring(initialize) )
import Enigma ( connect )
import Language ( Letter(A, B) )
import Control.Monad.State.Strict ( runState )
--import Control.Monad.Reader (asks, runReader)
--import Diagrams.TwoD.Arrow ( arrowBetween )
--import Diagrams.Backend.Cairo.CmdLine ( B )
--import Bombe.Wiring.WiringDiagram ( drawWires, drawWiresSymmetric, DiagramShape(..), BlockShape(..) )
import Bombe.Wiring.MatrixWiring.MatrixWiringStandard
    ( MatrixWiringStandard )
{-import Diagrams
    ( fcA,
      hsep,
      vsep,
      rect,
      text,
      rotateBy,
      translateX,
      translateY,
      p2,
      (#),
      Diagram )-}
{-import Diagrams.Prelude
    ( withOpacity, green, lightgray, orange, white )-}
main :: IO ()
main = do
    putStrLn "We start with a simple4 enigma"
    let
        enigma = simple4
        wiring = initialize 4 :: MatrixWiringStandard
    print $ runState (Enigma.connect wiring A B) enigma


{-
explainingClosure :: Diagram B
explainingClosure =
    let
        sep = 16.0
        enigma = simple4
        wiring = initialize 4 :: MatrixWiringStandard
        initialDiagram = drawWires wiring
        connectAB      = drawWiresSymmetric $ evalState (
            do
                Enigma.connect wiring A B) enigma
        connectABClosed = drawWiresSymmetric $ evalState (
            do
                w1 <- Enigma.connect wiring A B
                return $ closure w1) enigma
        connectCD      = drawWiresSymmetric $ evalState (
            do
                Enigma.connect wiring C D) enigma
        connectCDClosed = drawWiresSymmetric $ evalState (
            do
                w1 <- Enigma.connect wiring C D
                return $ closure w1) enigma
        connectABCD = drawWiresSymmetric $ evalState (
            do
                w1 <- Enigma.connect wiring A B
                Enigma.connect w1 C D) enigma
        connectABCDClosed = drawWiresSymmetric $ evalState (
            do
                w1 <- Enigma.connect wiring A B
                w2 <- Enigma.connect w1 C D
                return $ closure w2) enigma

        connectArrowAB = do 
            height <- asks dHeight
            dWidth <- asks dWidth
            --let width = sep
            return $  
                (text "Connect A to B" # translateX (dWidth/5.0) # translateY (3.0) <>  
                arrowBetween (p2 (0.0, 0.0)) (p2 (sep, 0.0)))
                    # translateX dWidth
                    # translateY (-height/2.0)

        closureArrowAB = do 
            height <- asks dHeight
            dWidth <- asks dWidth
            --let width = sep
            return $ 
                (text "T. Closure" # translateX (dWidth/5.0) # translateY (3.0) <>  
                arrowBetween (p2 (0.0, 0.0)) (p2 (sep, 0.0)))
                    # translateX (2*dWidth+sep)
                    # translateY (-height/2.0)

        connectAndClosureArrowAB = do 
            height <- asks dHeight
            dWidth <- asks dWidth
            --let width = sep
            return $ 
                (text "Connect A to B and T. Closure" # translateX (3*dWidth/4) # translateY (3.0) <>  
                arrowBetween (p2 (0.0, 0.0)) (p2 (dWidth+2*sep, 0.0)))
                    # translateX dWidth
                    # translateY (-2*height-2*sep-height/2.0)


        connectArrowCD = do 
            height <- asks dHeight
            width <- asks dWidth
            --let height = sep
            return $  
                (rotateBy (1/4) (text "Connect C to D") # translateY (-height/5.0) # translateX (-3.0) <>  
                arrowBetween (p2 (0.0, 0.0)) (p2 (0.0, -sep)))
                    # translateX (width/2.0)
                    # translateY (-height)

        closureArrowCD = do 
            height <- asks dHeight
            width <- asks dWidth
            --let height = sep
            return $  
                (rotateBy (1/4) (text "T. Closure") # translateY (-height/5.0) # translateX (-3.0) <>  
                arrowBetween (p2 (0.0, 0.0)) (p2 (0.0, -sep)))
                    # translateX (width/2.0)
                    # translateY (-2*height-sep)
            --return $ arrowBetween (p2 (width/2.0, -2*height-sep)) (p2 (width/2.0, -2*height-2*sep))

        connectAndClosureArrowCD = do 
            height <- asks dHeight
            width <- asks dWidth
            --let height = sep
            return $  
                (rotateBy (1/4) (text "Connect C to D and T. Closure") # translateY (-3*height/4) # translateX (-3.0) <>  
                arrowBetween (p2 (0.0, 0.0)) (p2 (0.0, -height-2*sep)))
                    # translateX (2*width+2*sep+width/2.0)
                    # translateY (-height)
            --return $ arrowBetween (p2 (0.0, 0.0)) (p2 (0.0, -height-2*sep))
        



        shape = DiagramShape
            40.0
            40.0
            4.0
            4.0
            (BlockShape
                (green `withOpacity` 1.0)
                (white `withOpacity` 1.0)
                True)
            (lightgray `withOpacity` 1.0)
        todo = do
                iD <- initialDiagram
                abD <- connectAB
                abCD <- connectABClosed
                cdD <- connectCD
                cdCD <- connectCDClosed
                abcdD <- connectABCD
                abcdCD <- connectABCDClosed
                leftAir <- asks leftAir 
                upperAir <- asks upperAir
                height <- asks dHeight
                width <- asks dWidth

                cABD  <- connectArrowAB
                ctABD <- closureArrowAB
                ccABD <- connectAndClosureArrowAB
                cCDD  <- connectArrowCD
                ctCDD <- closureArrowCD
                ccCDD <- connectAndClosureArrowCD

                return $ 
                    (vsep sep [
                        hsep sep [iD, abD, abCD],
                        hsep sep [cdD, abcdD],
                        hsep sep [cdCD]
                    ]) #translateX (leftAir) # translateY (-upperAir) <>
                    abcdCD # translateX (2*(width+sep)+leftAir) # translateY (-2*(height+sep)-upperAir) <>
                    cABD #translateY (-upperAir/2.0) <>
                    cABD #translateY (-upperAir/2.0-height-sep) <>
                    ctABD #translateY (-upperAir/2.0) <>
                    ccABD #translateY (-upperAir/2.0) <>
                    cCDD #translateX (leftAir/2.0) <>
                    cCDD #translateX (leftAir/2.0+width+sep) <>
                    ctCDD #translateX (leftAir/2.0) <>
                    ccCDD #translateX (leftAir/2.0) <>
                    rect (3*width + 3*sep) (3*height + 3*sep) 
                        # fcA (orange `withOpacity` 1.0)
                        # translateX ((3*width+2*sep)/2.0)
                        # translateY (-(3*height+2*sep)/2.0)


    in
        runReader todo shape
-}


















{- 
exploreQR :: Diagram B --MatrixWiring -> Diagram B
exploreQR = --(MatrixWiring matrix n) =
    let

        --(q,r) = qr 

        sep = 16.0
        enigma = simple4
        n = 4
        wiring = initialize n :: MatrixWiring
        initialDiagram = 
            do  
                let abMatrix  = wiring
                let (q,r)     = qr (getMatrix abMatrix)
                abDefault    <- drawWires abMatrix
                abq          <- drawWires (MatrixWiring q n)
                abr          <- drawWires (MatrixWiring r n)
                abClosed     <- drawWires (closure abMatrix)
                return $ vsep sep [abDefault, abq, abr, abClosed] 
        
        ab = 
            do  
                let abMatrix  = evalState (do Enigma.connect wiring A B) enigma
                let (q,r)     = qr (getMatrix abMatrix)
                abDefault    <- drawWires abMatrix
                abq          <- drawWires (MatrixWiring q n)
                abr          <- drawWires (MatrixWiring r n)
                abClosed     <- drawWires (closure abMatrix)
                return $ vsep sep [abDefault, abq, abr, abClosed] 

        cd = 
            do  
                let cdMatrix  = evalState (do Enigma.connect wiring C D) enigma
                let (q,r)     = qr (getMatrix cdMatrix)
                cdDefault    <- drawWires cdMatrix
                cdq          <- drawWires (MatrixWiring q n)
                cdr          <- drawWires (MatrixWiring r n)
                cdClosed     <- drawWires (closure cdMatrix)
                return $ vsep sep [cdDefault, cdq, cdr, cdClosed] 

        abcd = 
            do  
                let abcdMatrix  = evalState ( do 
                        w1 <- Enigma.connect wiring A B
                        Enigma.connect w1 C D
                        ) enigma
                let (q,r)     = qr (getMatrix abcdMatrix)
                abcdDefault    <- drawWires abcdMatrix
                abcdq          <- drawWires (MatrixWiring q n)
                abcdr          <- drawWires (MatrixWiring r n)
                abcdClosed     <- drawWires (closure abcdMatrix)
                return $ vsep sep [abcdDefault, abcdq, abcdr, abcdClosed]

        shape = DiagramShape
            40.0
            40.0
            4.0
            4.0
            (BlockShape
                (green `withOpacity` 1.0)
                (white `withOpacity` 1.0)
                True)
            (lightgray `withOpacity` 1.0)
        todo = do
                iD    <- initialDiagram
                abD   <- ab
                cdD   <- cd
                abcdD <- abcd

                leftAir  <- asks leftAir 
                upperAir <- asks upperAir
                height   <- asks dHeight
                width    <- asks dWidth

                return $ 
                    hsep sep [iD, abD, cdD, abcdD]
                    
                    
                    -- #translateX (leftAir) # translateY (-upperAir) <>
                    --abcdCD # translateX (2*(width+sep)+leftAir) # translateY (-2*(height+sep)-upperAir) <>

                    --rect (3*width + 3*sep) (3*height + 3*sep) 
                    --    # fcA (orange `withOpacity` 1.0)
                    --    # translateX ((3*width+2*sep)/2.0)
                    --    # translateY (-(3*height+2*sep)/2.0)


    in
        runReader todo shape-}