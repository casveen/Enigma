
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}


module WiringSpec where
{- Spec for wiring, specifically the computation of transitive closure -}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Parts
    ( allPlugboards26,
      allReflectors26,
      allReflectors4,
      allReflectors6,
      allRotors26,
      allRotors4,
      allRotors6,
      beta,
      i,
      identityPlugboard,
      ii,
      iii,
      im6,
      thinReflectorC,
      ukw,
      ukwm6,
      v,
      vi,
      viii,
      simple6 )
import Language
import Test.Hspec
import Test.QuickCheck hiding ((><))
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Transform
import Enigma
import Plugboard
import Cartridge
import Cipher
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Numeric.LinearAlgebra
import Debug.Trace
import Data.TypeNums
import Numeric.LinearAlgebra.Data
import GHC.Num.Natural
import Bombe.Wiring -- hiding (ModTwo)

import Control.Exception
import Test.HUnit.Lang (HUnitFailure(..))



import Prelude hiding ((<>))
--import Language
--import Data.TypeNums
import Numeric.LinearAlgebra.Data
--import GHC.Num.Natural




--type ModTwo = Numeric.LinearAlgebra.Data.Mod 2 Z
--type WM = Matrix ModTwo
--type M2 = Herm ModTwo -- hermetic matrix of integers modulo two


wiringSpec :: SpecWith ()
wiringSpec = describe "Testing wiring spec." $ context "given a wiring board" $ do
    let n = 4
    let wiring = Bombe.Wiring.initialize n :: MatrixWiringC
    let wires  = chooseInt (0,n*n-1)
    it "should connect two wires" $
        forAll wires $ \wireFrom ->
        forAll wires $ \wireTo ->
            let
                bwFrom = wireToBundleWire wireFrom n
                bwTo   = wireToBundleWire wireTo n
                connectedMatrix = connectWire wiring bwFrom bwTo
            in
                isConnectedBW connectedMatrix bwFrom bwTo `shouldBe` True

    it "should connect two wires symmetrically" $
        forAll wires $ \wireFrom ->
        forAll wires $ \wireTo ->
            let
                bwFrom = wireToBundleWire wireFrom n
                bwTo   = wireToBundleWire wireTo n
                connectedMatrix = connectWire wiring bwFrom bwTo
            in
                isConnectedBW connectedMatrix bwTo bwFrom `shouldBe` True

    it "connect 3 wires transitively" $
        forAll wires $ \wire1 ->
        forAll wires $ \wire2 ->
        forAll wires $ \wire3 ->
            let
                bw1 = wireToBundleWire wire1 n
                bw2 = wireToBundleWire wire2 n
                bw3 = wireToBundleWire wire3 n
                connectedMatrix = closure $ connectWire (connectWire wiring bw1 bw2) bw2 bw3

            in
                --if bw1 /= bw2 && bw2 /= bw3 
                    --then 
                        isConnectedBW connectedMatrix bw1 bw3 `shouldBe` True
                    --else return ()
    modifyMaxSuccess (const 10000) $ it "connect 6 wires transitively" $ 
        forAll wires $ \wire1 ->
        forAll wires $ \wire2 ->
        forAll wires $ \wire3 ->
        forAll wires $ \wire4 ->
        forAll wires $ \wire5 ->
        forAll wires $ \wire6 ->
            let
                bw1 = wireToBundleWire wire1 n
                bw2 = wireToBundleWire wire2 n
                bw3 = wireToBundleWire wire3 n
                bw4 = wireToBundleWire wire4 n
                bw5 = wireToBundleWire wire5 n
                bw6 = wireToBundleWire wire6 n
                connectedMatrix = closure $
                    connectWire (
                        connectWire (
                            connectWire (
                                connectWire (
                                    connectWire wiring bw1 bw2)
                                    bw2 bw3
                                    )
                                bw3 bw4
                                )
                            bw4 bw5
                            )
                        bw5 bw6
            in 
                if bw1 /= bw2 && bw2 /= bw3 && bw3 /= bw4 && bw4 /= bw5 && bw5 /= bw6 then 
                do
                    isConnectedBW connectedMatrix bw6 bw1 `shouldBe` True
                    isConnectedBW connectedMatrix bw6 bw2 `shouldBe` True
                    isConnectedBW connectedMatrix bw6 bw3 `shouldBe` True
                    isConnectedBW connectedMatrix bw6 bw4 `shouldBe` True
                    isConnectedBW connectedMatrix bw6 bw5 `shouldBe` True
                    isConnectedBW connectedMatrix bw1 bw3 `shouldBe` True
                    isConnectedBW connectedMatrix bw1 bw4 `shouldBe` True
                    isConnectedBW connectedMatrix bw1 bw5 `shouldBe` True
                    isConnectedBW connectedMatrix bw1 bw6 `shouldBe` True
                    isConnectedBW connectedMatrix bw2 bw4 `shouldBe` True
                    isConnectedBW connectedMatrix bw2 bw5 `shouldBe` True
                    isConnectedBW connectedMatrix bw2 bw6 `shouldBe` True
                    isConnectedBW connectedMatrix bw3 bw5 `shouldBe` True
                    isConnectedBW connectedMatrix bw3 bw6 `shouldBe` True
                    isConnectedBW connectedMatrix bw4 bw6 `shouldBe` True
                else return ()

transitiveClosureSpec :: SpecWith ()
transitiveClosureSpec = describe "Testing transitive closure spec." $ do
    context "given a transition matrix where all states connected" $ do
        let matrix = (4><4) [1,1,0,0,  1,1,1,0,  0,1,1,1,  0,0,1,1] :: WM
        it "should give 1 as transitive closure" $
            transitiveClosure matrix `shouldBe`  (4><4) [1,1,1,1,  1,1,1,1,  1,1,1,1,  1,1,1,1]
    context "given a bipartite transition matrix" $ do
        let matrix = (4><4) [1,1,0,0,  1,1,0,0,  0,0,1,1,  0,0,1,1] :: WM
        it "should give 1 in two blocks as transitive closure" $
            transitiveClosure matrix `shouldBe`  (4><4) [1,1,0,0,  1,1,0,0,  0,0,1,1,  0,0,1,1]
    context "given a tripartite, sparse transition matrix" $ do
        let matrix = (6><6) [1,0,1,0,0,0,  0,1,0,0,0,1,  1,0,1,0,1,0,  0,0,0,1,0,0,  0,0,1,0,1,0,  0,1,0,0,0,1] :: WM
        it "should add 1 connection in closure" $
            transitiveClosure matrix `shouldBe`  (6><6) [1,0,1,0,1,0,  0,1,0,0,0,1,  1,0,1,0,1,0,  0,0,0,1,0,0,  1,0,1,0,1,0,  0,1,0,0,0,1]
    context "given a transition matrix where all states connect haphazardly" $ do
        let matrix = (6><6) [1,0,0,0,1,0,  0,1,0,1,1,0,  0,0,1,0,0,1,  0,1,0,1,0,1,  1,1,0,0,1,0,  0,0,1,1,0,1] :: WM
        it "should give 1 as transitive closure" $
            transitiveClosure matrix `shouldBe`  (6><6) [1,1,1,1,1,1,  1,1,1,1,1,1,  1,1,1,1,1,1,  1,1,1,1,1,1,  1,1,1,1,1,1,  1,1,1,1,1,1]
    context "given a transition matrix where all states connect haphazardly from initial" $ do
        let matrix = (9><9) 
                [1,1,0,1,0,0,0,0,0, 
                 0,1,1,0,1,0,0,0,0,
                 0,0,1,0,0,0,0,0,0,
                 0,0,0,1,0,0,1,1,0,
                 0,0,0,0,1,1,0,0,0,
                 0,0,0,0,0,1,0,0,0,
                 0,0,0,0,0,0,1,0,0,
                 0,0,0,0,0,0,0,1,1,
                 0,0,0,0,0,0,0,0,1]
        it "should give a closure where 1 reaches all" $
            transitiveClosure matrix `shouldBe`  
            (9><9) 
                [1,1,1,1,1,1,1,1,1, 
                 0,1,1,0,1,1,0,0,0,
                 0,0,1,0,0,0,0,0,0,
                 0,0,0,1,0,0,1,1,1,
                 0,0,0,0,1,1,0,0,0,
                 0,0,0,0,0,1,0,0,0,
                 0,0,0,0,0,0,1,0,0,
                 0,0,0,0,0,0,0,1,1,
                 0,0,0,0,0,0,0,0,1] 
    context "given a transition matrix where all states connect haphazardly from initial in symmetry" $ do
        let matrix = (9><9) 
                [1,1,0,1,0,0,0,0,0, 
                 1,1,1,0,1,0,0,0,0,
                 0,1,1,0,0,0,0,0,0,
                 1,0,0,1,0,0,1,1,0,
                 0,1,0,0,1,1,0,0,0,
                 0,0,0,0,1,1,0,0,0,
                 0,0,0,1,0,0,1,0,0,
                 0,0,0,1,0,0,0,1,1,
                 0,0,0,0,0,0,0,1,1]
        it "should give 1" $
            transitiveClosure matrix `shouldBe`  
            (9><9) 
                [1,1,1,1,1,1,1,1,1, 
                 1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1,
                 1,1,1,1,1,1,1,1,1] 
            


