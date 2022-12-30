{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module WiringSpec (
    wiringSpec,
    transitiveClosureSpec
) where

{- Spec for wiring, specifically the computation of transitive closure -}

import Test.Hspec ( shouldBe, it, context, describe, SpecWith )
import Test.QuickCheck ( forAll, chooseInt )
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Numeric.LinearAlgebra ( (><) )
import Bombe.Wiring.Wiring
    ( wireToBundleWire,
      Wiring(isConnectedBW, getLetters, closure, connectWire) ) -- hiding (ModTwo)
import Bombe.Wiring.MatrixWiring.MatrixWiring(WM)
import Prelude hiding ((<>))
import Bombe.Wiring.TransitiveClosure ( transitiveClosure )
import qualified Control.Monad

wiringSpec :: (Wiring w) => w -> SpecWith ()
wiringSpec wiring = describe "Testing wiring spec." $ context "given a wiring board" $ do
    let n = getLetters wiring
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
                isConnectedBW connectedMatrix bw1 bw3 `shouldBe` True

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
                (Control.Monad.when (bw1 /= bw2 && bw2 /= bw3 && bw3 /= bw4 && bw4 /= bw5 && bw5 /= bw6) $
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
                isConnectedBW connectedMatrix bw4 bw6 `shouldBe` True)







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