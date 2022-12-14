{-# OPTIONS_GHC -Wno-name-shadowing #-}

import Parts
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












reencryptionProp :: (Show a, Cipher c, Ord a, Enum a) => c a -> a -> Property
reencryptionProp c p = decrypt c (encrypt c p) === p

main :: IO ()
main = hspec $ do
    tracedEncryptionSpec
    transformSpec
    cartridgeSpec
    enigmaSpec
    


{-
TEST_CASE("Testing a concrete example; encryption of a sample of the Donitz "
          "message") {
    // test if the enigma can encrypt/decrypt a portion of the Karl donitz
    // message
    // Reflector ThinReflectorC= Reflector("RDOBJNTKVEHMLFCWZAXGYIPSUQ");
    Enigma enigma({VIII, VI, V, BETA}, THINREFLECTORC);
    enigma.set_plugboard("AE.BF.CM.DQ.HU.JN.LX.PR.SZ.VW");
    enigma.set_ring_setting("EPEL");
    enigma.set_rotor_position("CDSZ");
    string encryption=
        enigma.encrypt("LANOTCTOUARBBFPMHPHGCZXTDYGAHGUFXGEWKBLKGJWLQXXTGPJJAVTOCKZFSLPPQIHZFX"
                       "OEBWIIEKFZLCLOAQJULJOYHSSMBBGWHZANVOIIPYRBRTDJQDJJOQKCXWDNBBTYVXLYTAPG"
                       "VEATXSONPNYNQFUDBBHHVWEPYEYDOHNLXKZDNWRHDUWUJUMWWVIIWZXIVIUQDRHYMNCYEF"
                       "UAPNHOTKHKGDNPSAKNUAGHJZSMJBMHVTREQEDGXHLZWIFUSKDQVELNMIMITHBHDBWVHDFY"
                       "HJOQIHORTDJDBWXEMEAYXGYQXOHFDMYUXXNOJAZRSGHPLWMLRECWWUTLRTTVLBHYOORGLG"
                       "OWUXNXHMHYFAACQEKTHSJW");
    REQUIRE(encryption == "KRKRALLEXXFOLGENDESISTSOFORTBEKANNTZUGEBENXXICHHABEFOLGELNBEBEFEHL"
                          "ERHALTENXXJANSTERLEDESBISHERIGXNREICHSMARSCHALLSJGOERINGJSETZTDERF"
                          "UEHRERSIEYHVRRGRZSSADMIRALYALSSEINENNACHFOLGEREINXSCHRIFTLSCHEVOLL"
                          "MACHTUNTERWEGSXABSOFORTSOLLENSIESAEMTLICHEMASSNAHMENVERFUEGENYDIES"
                          "ICHAUSDERGEGENWAERTIGENLAGEERGEBENXGEZXREICHSLEITEIKKTULPEKKJBORMA"
                          "NNJXXOBXDXMMMDURNHFKSTXKOMXADMXUUUBOOIEXKP");
}






-}