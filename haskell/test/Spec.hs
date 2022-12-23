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
import Test.QuickCheck
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Transform
import Enigma
import Plugboard
import Cartridge
import Cipher
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Enigma (encryptTraversableOfMonads)
import Bombe.Wiring (EagerMatrixWiring)

transformSpec :: SpecWith ()
transformSpec =
    describe "Testing transform spec." $ context "given a specific transform" $ do
    let t = transformFromLanguage (readLetters "BFADCE") -- :: Transform Letter
    it "should encrypt by applying the transform as specified" $ do
        let result = map (encrypt t) [A,B,C,D,E,F]
        let expected = [B,F,A,D,C,E]
        result `shouldBe` expected
    it "should decrypt by applying the transform as specified" $ do
        let result = map (decrypt t) [A,B,C,D,E,F]
        let expected = [C,A,E,D,F,B]
        result `shouldBe` expected




cartridgeSpec :: SpecWith ()
cartridgeSpec = describe "Testing cartridge spec." $ do
    --make an arbitrary cartridge

    --test it for reencryption
    context "given an arbitrary cartridge" $ modifyMaxSuccess (const 1000) $ do
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors26) <*> elements allReflectors26 <*> arbitrary
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryCartridge $ \c ->
            forAll (chooseEnum (A, Z)) $ \p ->
                reencryptionProp c p
    
    context "given an arbitrary minimal cartridge" $ modifyMaxSuccess (const 100) $ do
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors4) <*> elements allReflectors4 <*> arbitrary
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryCartridge $ \c ->
            forAll (chooseEnum (A, D)) $ \p ->
                reencryptionProp c p
    
    context "given an arbitrary 6 wired cartridge" $ modifyMaxSuccess (const 100) $ do
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors6) <*> elements allReflectors6 <*> arbitrary
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryCartridge $ \c ->
            forAll (chooseEnum (A, F)) $ \p ->
                reencryptionProp c p

    context "given a specific cartridge (i, ukw)" $ do
        let specificCartridge = Cartridge [i] ukw [0]
        it "should encrypt in a given pattern" $
            map (encrypt specificCartridge) [A .. Z] `shouldBe` [F,R,L,K,J,A,M,U,V,E,D,C,G,S,W,Y,X,B,N,Z,H,I,O,Q,P,T]

    context "given a specific cartridge (im6, ukw) with 6 letters" $ do
        let specificCartridge = Cartridge [im6] ukwm6 [0]
        it "should encrypt in a given pattern" $
            map (encrypt specificCartridge) [A .. F] `shouldBe` [F,C,B,E,D,A]

    {-context "given a specific turned cartridge (im6+1, ukw) with 6 letters" $ do
        let specificCartridge = Cartridge [im6] ukwm6 [1]
        it "should encrypt in a given pattern" $
            map (encrypt specificCartridge) [A .. F] `shouldBe` [F,C,B,E,D,A]  
    
    context "given a specific turned cartridge (im6+2, ukw) with 6 letters" $ do
        let specificCartridge = Cartridge [im6] ukwm6 [2]
        it "should encrypt in a given pattern" $
            map (encrypt specificCartridge) [A .. F] `shouldBe` [D,E,F,A,B,C]  -}

donitzCipherText :: String
donitzCipherText = "LANOTCTOUARBBFPMHPHGCZXTDYGAHGUFXGEWKBLKGJWLQXXTGPJJAVTOCKZFSLPPQIHZFX" ++
                              "OEBWIIEKFZLCLOAQJULJOYHSSMBBGWHZANVOIIPYRBRTDJQDJJOQKCXWDNBBTYVXLYTAPG" ++
                              "VEATXSONPNYNQFUDBBHHVWEPYEYDOHNLXKZDNWRHDUWUJUMWWVIIWZXIVIUQDRHYMNCYEF" ++
                              "UAPNHOTKHKGDNPSAKNUAGHJZSMJBMHVTREQEDGXHLZWIFUSKDQVELNMIMITHBHDBWVHDFY" ++
                              "HJOQIHORTDJDBWXEMEAYXGYQXOHFDMYUXXNOJAZRSGHPLWMLRECWWUTLRTTVLBHYOORGLG" ++
                              "OWUXNXHMHYFAACQEKTHSJW"

enigmaSpec :: SpecWith ()
enigmaSpec = describe "Testing enigma spec." $ do

    --make an arbitrary cartridge
    let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors26)
                                       <*> elements allReflectors26
                                       <*> vectorOf 3 (chooseInt (0,25))
    let arbitraryPlugboard = elements allPlugboards26
    let arbitraryEnigmaState = Enigma <$> arbitraryPlugboard <*> arbitraryCartridge --arbitraryenigmaSTATE
    let arbitraryEnigma = arbitraryEnigmaState
    let arbitraryText = listOf (chooseEnum (A, Z))

    context "given an arbitrary enigmastate" $ modifyMaxSuccess (const 10000) $
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryEnigmaState $ \c ->
            forAll (chooseEnum (A, Z)) $ \p ->
                reencryptionProp c p

    describe "given a specific enigma" $ do
        let cartridge = Cartridge [iii,ii,i] ukw [0,0,0]
        let enigma    = Enigma identityPlugboard cartridge
        let correctSequence = [[A,D,U],[A,D,V],[A,E,W],[B,F,X],[B,F,Y]]
        it "should step in the correct sequence when encrypting several letters" $ do
            let actualSequence =
                            let
                                action = do
                                    setRotorPosition [A,A,A] [A,D,U]
                                    adu <- getRotorPosition
                                    step
                                    adv <- getRotorPosition
                                    step
                                    aew <- getRotorPosition
                                    step
                                    bfx <- getRotorPosition
                                    step
                                    bfy <- getRotorPosition
                                    return $ fmap (fmap toEnum) [adu,adv,aew,bfx,bfy]
                            in
                                evalState action enigma
            actualSequence `shouldBe` correctSequence

        let cartridge = Cartridge [i,ii,iii] ukw [0,0,0]
        let enigma    = Enigma identityPlugboard cartridge
        let correctSequence = [[K,D,O],[K,D,P],[K,D,Q],[K,E,R],[L,F,S],[L,F,T],[L,F,U]]
        it "should double-step when third rotor engages" $ do
            let actualSequence =
                            let
                                action = do
                                    setRotorPosition [A,A,A] [K,D,O]
                                    kdo <- getRotorPosition
                                    step
                                    kdp <- getRotorPosition
                                    step
                                    kdq <- getRotorPosition
                                    step
                                    ker <- getRotorPosition
                                    step
                                    lfs <- getRotorPosition
                                    step
                                    lft <- getRotorPosition
                                    step
                                    lfu <- getRotorPosition
                                    return $ fmap (fmap toEnum) [kdo, kdp, kdq, ker, lfs, lft, lfu]
                            in
                                evalState action enigma
            actualSequence `shouldBe` correctSequence

    context "given an arbitrary enigma" $ modifyMaxSuccess (const 10000) $ do
        it "should return the original plaintext upon enc- then decryption of arbitrary text" $ forAll arbitraryEnigma $ \c ->
            forAll arbitraryText $ \p ->
                let left = evalState (encryptText (evalState (encryptText p) c)) c
                in  left === p
        it "should return the original plaintext upon enc- then decryption of a single letter" $ forAll arbitraryEnigma $ \c ->
            forAll (chooseEnum (A, Z)) $ \p ->
                let left = evalState (enc (evalState (enc p) c)) c
                in  left === p

    context "given the enigma from the donitz example" $ do
        let cartridge = Cartridge [viii,vi,v,beta] thinReflectorC [0,0,0]
        let plugboard = mkPlugboard [(A,E),(B,F),(C,M),(D,Q),(H,U),(J,N),(L,X),(P,R),(S,Z),(V,W),(G,G),(I,I),(K,K),(O,O),(T,T),(Y,Y)]
        let enigma    = Enigma plugboard cartridge
        it "should reencrypt an arbitrary text to itself" $ forAll arbitraryText $ \p ->
            let left = evalState (encryptText (evalState (encryptText p) enigma)) enigma
            in  left === p
        it "should decrypt the ciphered text to the donitz message" $ do
            let action = do
                    setRotorPosition [E,P,E,L] [C,D,S,Z]
                    let cipherText = donitzCipherText
                    encryptText $ stringToLanguage cipherText
            let decrypted = evalState action enigma
            decrypted `shouldBe` stringToLanguage ("KRKRALLEXXFOLGENDESISTSOFORTBEKANNTZUGEBENXXICHHABEFOLGELNBEBEFEHL" ++
                                 "ERHALTENXXJANSTERLEDESBISHERIGXNREICHSMARSCHALLSJGOERINGJSETZTDERF" ++
                                 "UEHRERSIEYHVRRGRZSSADMIRALYALSSEINENNACHFOLGEREINXSCHRIFTLSCHEVOLL" ++
                                 "MACHTUNTERWEGSXABSOFORTSOLLENSIESAEMTLICHEMASSNAHMENVERFUEGENYDIES" ++
                                 "ICHAUSDERGEGENWAERTIGENLAGEERGEBENXGEZXREICHSLEITEIKKTULPEKKJBORMA" ++
                                 "NNJXXOBXDXMMMDURNHFKSTXKOMXADMXUUUBOOIEXKP")

tracedEncryptionSpec :: SpecWith ()
tracedEncryptionSpec =
    describe "Testing traced encryption spec." $ context "given a specific enigma" $ do
    let cartridge = Cartridge [i] ukw [0]
    let plugboard = mkPlugboard [(A,E),(B,F),(C,M),(D,Q),(H,U),(J,N),(L,X),(P,R),(S,Z),(V,W),(G,G),(I,I),(K,K),(O,O),(T,T),(Y,Y)]
    let enigma    = Enigma plugboard cartridge
    it "should properly trace encryptions" $ do
        let correctWriter = writer (N, [A,E,L,Z,J,N])
        let actualWriter  = tracedEncrypt enigma A
        actualWriter `shouldBe` correctWriter
        --A `shouldBe` B

undeterministicEnigmaSpec :: SpecWith ()
undeterministicEnigmaSpec = describe "Testing undeterministic enigma spec." $ do
    context "given a specific enigma" $ do
        let enigma              = simple6
        let undeterministicText = [[A,B,C,D,E,F],[A,B,C,D,E,F]]
        it "should encrypt all letters undeterministically" $
            evalState (encryptTraversableOfMonads undeterministicText) enigma `shouldBe`
            [[B,A,E,F,C,D],[D,F,E,A,C,B]]

    context "given an arbitrary enigma" $ modifyMaxSuccess (const 1000) $ do     
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors26)
                                        <*> elements allReflectors26
                                        <*> vectorOf 3 (chooseInt (0,25))
        let arbitraryPlugboard = elements allPlugboards26
        let arbitraryEnigmaState = Enigma <$> arbitraryPlugboard <*> arbitraryCartridge
        let arbitraryEnigma = arbitraryEnigmaState
        let arbitraryText = listOf (listOf (chooseEnum (A, Z)))
        it "should reencrypt all letters undeterministically in same order" $
            forAll arbitraryEnigmaState $ \c ->
            forAll arbitraryText $ \p ->  
                evalState (encryptTraversableOfMonads (evalState (encryptTraversableOfMonads p) c)) c `shouldBe` p



{-rotorSpec = 
    describe "Testing rotor spec." $ do
        context "given an arbitrary rotor" $ do
            let rotors = elements allRotors26
            let letters = chooseEnum (A,Z)
            let offsets = chooseInt (0,25)
            let n=26
            it "should use the same wire if plaintext and rotor turns" $ do
                forAll rotors $ \r -> 
                    forAll letters $ \l ->
                        forAll offsets $ \o ->
                            shiftLetter (encrypt r $ shiftLetter l o n) (-o) n-}



wiringSpec :: SpecWith ()
wiringSpec = describe "Testing wiring spec." $ do
    context "given a wiring board" $ do
        let wiring = Bombe.Wiring.initialize 26 :: EagerMatrixWiring
        let wires  = chooseInt (0,26*26-1)
        it "should connect two wires" $
            forAll wires $ \wireFrom ->
            forAll wires $ \wireTo ->
                let 
                    bwFrom = wireToBundleWire wireFrom 26
                    bwTo   = wireToBundleWire wireTo 26
                    connectedMatrix = connectWire wiring bwFrom bwTo
                in 
                    isConnected connectedMatrix bwFrom bwTo `shouldBe` True
        it "should connect two wires symmetrically" $
            forAll wires $ \wireFrom ->
            forAll wires $ \wireTo ->
                let 
                    bwFrom = wireToBundleWire wireFrom 26
                    bwTo   = wireToBundleWire wireTo 26
                    connectedMatrix = connectWire wiring bwFrom bwTo
                in 
                    isConnected connectedMatrix bwTo bwFrom `shouldBe` True
        it "connect 3 wires transitively" $
            forAll wires $ \wire1 ->
            forAll wires $ \wire2 ->
            forAll wires $ \wire3 ->
                let 
                    bw1 = wireToBundleWire wire1 26
                    bw2 = wireToBundleWire wire2 26
                    bw3 = wireToBundleWire wire3 26
                    connectedMatrix = closure $ connectWire (connectWire wiring bw1 bw2) bw2 bw3

                in 
                    isConnected connectedMatrix bw1 bw3 `shouldBe` True
        it "connect 6 wires transitively" $
            forAll wires $ \wire1 ->
            forAll wires $ \wire2 ->
            forAll wires $ \wire3 ->
            forAll wires $ \wire4 ->
            forAll wires $ \wire5 ->
            forAll wires $ \wire6 ->
                let 
                    bw1 = wireToBundleWire wire1 26
                    bw2 = wireToBundleWire wire2 26
                    bw3 = wireToBundleWire wire3 26
                    bw4 = wireToBundleWire wire4 26
                    bw5 = wireToBundleWire wire5 26
                    bw6 = wireToBundleWire wire6 26
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
                    do 
                        isConnected connectedMatrix bw6 bw1 `shouldBe` True
                        isConnected connectedMatrix bw6 bw2 `shouldBe` True
                        isConnected connectedMatrix bw6 bw3 `shouldBe` True
                        isConnected connectedMatrix bw6 bw4 `shouldBe` True
                        isConnected connectedMatrix bw6 bw5 `shouldBe` True
                        isConnected connectedMatrix bw1 bw3 `shouldBe` True
                        isConnected connectedMatrix bw1 bw4 `shouldBe` True
                        isConnected connectedMatrix bw1 bw5 `shouldBe` True
                        isConnected connectedMatrix bw1 bw6 `shouldBe` True
                        isConnected connectedMatrix bw2 bw4 `shouldBe` True
                        isConnected connectedMatrix bw2 bw5 `shouldBe` True
                        isConnected connectedMatrix bw2 bw6 `shouldBe` True
                        isConnected connectedMatrix bw3 bw5 `shouldBe` True
                        isConnected connectedMatrix bw3 bw6 `shouldBe` True
                        isConnected connectedMatrix bw4 bw6 `shouldBe` True










reencryptionProp :: (Show a, Cipher c, Ord a, Enum a) => c a -> a -> Property
reencryptionProp c p = decrypt c (encrypt c p) === p

main :: IO ()
main = hspec $ do
    tracedEncryptionSpec
    transformSpec
    cartridgeSpec
    enigmaSpec
    undeterministicEnigmaSpec
    wiringSpec