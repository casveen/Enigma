{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

import Parts
    ( simple6,
      viii,
      vi,
      v,
      ukwm6,
      ukw,
      thinReflectorC,
      im6,
      iii,
      ii,
      identityPlugboard,
      i,
      beta,
      allRotors6,
      allRotors4,
      allRotors26,
      allReflectors6,
      allReflectors4,
      allReflectors26,
      allPlugboards26 )
import Test.Hspec
    ( shouldBe, it, context, describe, SpecWith, hspec )
import WiringSpec ( transitiveClosureSpec, wiringSpec )
import Test.QuickCheck
    ( forAll,
      chooseInt,
      Property,
      chooseEnum,
      elements,
      listOf,
      vectorOf,
      (===),
      Arbitrary(arbitrary), Gen )
import Test.Hspec.QuickCheck ( modifyMaxSuccess )
import Enigma
    ( EnigmaState(Enigma),
      enc,
      encryptText,
      encryptTraversableOfMonads,
      getRotorPosition,
      setRotorPosition,
      step )
import Plugboard ( mkPlugboard, Plugboard )
import Cartridge ( Cartridge(Cartridge) )
import Cipher
    ( Cipher(encrypt, decrypt, encryptEnummed, decryptEnummed), TraceableCipher(tracedEncrypt), Cipherable )
import Control.Monad.State.Strict ( evalState )
import Control.Monad.Writer.Strict ( MonadWriter(writer) )
import Bombe.Wiring.MatrixWiring.MatrixWiringStandard
    ( MatrixWiringStandard )
import Bombe.Wiring.MatrixWiring.MatrixWiringCompressed
    ( MatrixWiringCompressed )
import Bombe.Wiring.MatrixWiring.MatrixWiringLegacy
    ( MatrixWiringLegacy )
import Bombe.Wiring.Wiring ( Wiring(initialize) )
import Language(EnglishLetter, LetterOrdinal(..),readLetters,stringToLanguage, Letter6, Letter4)
import Transform
import Debug.Trace(trace)

reEnum :: (Enum a, Enum b) => a -> b
reEnum = toEnum . fromEnum

transformSpec :: SpecWith ()
transformSpec =
    describe "Testing transform spec." $ context "given a specific transform" $ do
    let t = transformFromLanguage (readLetters "BFADCE") :: Transform Letter6
    it "should encrypt by applying the transform as specified" $ do
        let result = map (encryptEnummed t) [A,B,C,D,E,F]
        let expected = [B,F,A,D,C,E]
        result `shouldBe` expected
    it "should decrypt by applying the transform as specified" $ do
        let result = map (decryptEnummed t) [A,B,C,D,E,F]
        let expected = [C,A,E,D,F,B]
        result `shouldBe` expected


cartridgeSpec :: SpecWith ()
cartridgeSpec = describe "Testing cartridge spec." $ do
    --make an arbitrary cartridge

    --test it for reencryption
    context "given an arbitrary cartridge" $ modifyMaxSuccess (const 1000) $ do
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors26) <*> elements allReflectors26 <*> arbitrary :: Gen (Cartridge EnglishLetter)
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryCartridge $ \c ->
            forAll (chooseEnum (minBound, maxBound)) $ \p ->
                reencryptionProp c p

    context "given an arbitrary minimal cartridge" $ modifyMaxSuccess (const 100) $ do
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors4) <*> elements allReflectors4 <*> arbitrary :: Gen (Cartridge Letter4)
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryCartridge $ \c ->
            forAll (chooseEnum (minBound, maxBound)) $ \p ->
                reencryptionProp c p

    context "given an arbitrary 6 wired cartridge" $ modifyMaxSuccess (const 100) $ do
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors6) <*> elements allReflectors6 <*> arbitrary :: Gen (Cartridge Letter6)
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryCartridge $ \c ->
            forAll (chooseEnum (minBound, maxBound)) $ \p ->
                reencryptionProp c p

    context "given a specific cartridge (i, ukw)" $ do
        let specificCartridge = Cartridge [i] ukw [0] :: Cartridge EnglishLetter
        it "should encrypt in a given pattern" $
            map (encrypt specificCartridge) [minBound .. maxBound] `shouldBe` map reEnum [F,R,L,K,J,A,M,U,V,E,D,C,G,S,W,Y,X,B,N,Z,H,I,O,Q,P,T]

    context "given a specific cartridge (im6, ukw) with 6 letters" $ do
        let specificCartridge = Cartridge [im6] ukwm6 [0] :: Cartridge Letter6
        it "should encrypt in a given pattern" $
            map (encrypt specificCartridge) [minBound .. maxBound] `shouldBe` map reEnum [F,C,B,E,D,A]

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
                                       <*> vectorOf 3 (chooseEnum (minBound,maxBound)) :: Gen (Cartridge EnglishLetter)
    let arbitraryPlugboard = elements allPlugboards26 :: Gen (Plugboard EnglishLetter)
    let arbitraryEnigmaState = Enigma <$> arbitraryPlugboard <*> arbitraryCartridge --arbitraryenigmaSTATE
    let arbitraryEnigma = arbitraryEnigmaState :: Gen (EnigmaState EnglishLetter)
    let arbitraryText = listOf (chooseEnum (A, Z))

    context "given an arbitrary enigmastate" $ modifyMaxSuccess (const 10000) $ do
        it "the cartridge should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryCartridge $ \c ->
                forAll (chooseEnum (minBound, maxBound)) $ \p ->
                    reencryptionProp c p
        it "the plugboard should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryPlugboard $ \c ->
            forAll (chooseEnum (minBound, maxBound)) $ \p ->
                reencryptionProp c p
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryEnigmaState $ \c ->
            forAll (chooseEnum (minBound, maxBound)) $ \p ->
                reencryptionProp c p

    describe "given a specific enigma" $ do
        let cartridge = Cartridge [iii,ii,i] ukw [0,0,0] :: Cartridge EnglishLetter
        let enigma    = Enigma identityPlugboard cartridge
        let correctSequence = map (map reEnum) [[A,D,U],[A,D,V],[A,E,W],[B,F,X],[B,F,Y]] -- :: [[EnglishLetter]]
        it "should step in the correct sequence when encrypting several letters" $ do
            let actualSequence =
                            let
                                action = do
                                    setRotorPosition (map reEnum [A,A,A]) (map reEnum [A,D,U])
                                    adu <- getRotorPosition
                                    step
                                    adv <- getRotorPosition
                                    step
                                    aew <- getRotorPosition
                                    step
                                    bfx <- getRotorPosition
                                    step
                                    bfy <- getRotorPosition
                                    return [adu,adv,aew,bfx,bfy]
                            in
                                evalState action enigma
            actualSequence `shouldBe` correctSequence

        let cartridge = Cartridge [i,ii,iii] ukw [0,0,0] :: Cartridge EnglishLetter
        let enigma    = Enigma identityPlugboard cartridge
        let correctSequence = map (map reEnum) [[K,D,O],[K,D,P],[K,D,Q],[K,E,R],[L,F,S],[L,F,T],[L,F,U]]
        it "should double-step when third rotor engages" $ do
            let actualSequence =
                            let
                                action = do
                                    setRotorPosition (map reEnum [A,A,A]) (map reEnum [K,D,O])
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
                                    return [kdo, kdp, kdq, ker, lfs, lft, lfu]
                            in
                                evalState action enigma
            actualSequence `shouldBe` correctSequence

    context "given an arbitrary enigma" $ modifyMaxSuccess (const 10000) $ do
        it "should return the original plaintext upon enc- then decryption of arbitrary text" $
            forAll arbitraryEnigma $ \c ->
                forAll arbitraryText $ \p ->
                    let
                        left = evalState (encryptText (evalState (encryptText (map reEnum p)) c)) c
                    in
                        left === map reEnum p
        it "should return the original plaintext upon enc- then decryption of a single letter" $
            forAll arbitraryEnigma $ \c ->
                forAll (chooseEnum (A, Z)) $ \p ->
                    let left = evalState (enc (evalState (enc (reEnum p)) c)) c
                    in  left === reEnum p

    context "given the enigma from the donitz example" $ do
        let cartridge = Cartridge [viii,vi,v,beta] thinReflectorC [0,0,0]
        let plugboard = mkPlugboard [(A,E),(B,F),(C,M),(D,Q),(H,U),(J,N),(L,X),(P,R),(S,Z),(V,W),(G,G),(I,I),(K,K),(O,O),(T,T),(Y,Y)]
        let enigma    = Enigma plugboard cartridge :: EnigmaState EnglishLetter
        it "should reencrypt an arbitrary text to itself" $ forAll arbitraryText $ \p ->
            let left = evalState (encryptText (evalState (encryptText (map reEnum p)) enigma)) enigma
            in  left === map reEnum p
        it "should decrypt the ciphered text to the donitz message" $ do
            let action = do
                    setRotorPosition (map reEnum [E,P,E,L]) (map reEnum [C,D,S,Z])
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
        let correctWriter = writer (reEnum N :: EnglishLetter, map reEnum [A,E,L,Z,J,N] :: [EnglishLetter])
        let actualWriter  = tracedEncrypt enigma (reEnum A)
        actualWriter `shouldBe` correctWriter
        --A `shouldBe` B

undeterministicEnigmaSpec :: SpecWith ()
undeterministicEnigmaSpec = describe "Testing undeterministic enigma spec." $ do
    context "given a specific enigma" $ do
        let enigma              = simple6
        let undeterministicText = map (map reEnum) [[A,B,C,D,E,F],[A,B,C,D,E,F]] :: [[Letter6]]
        it "should encrypt all letters undeterministically" $
            evalState (encryptTraversableOfMonads undeterministicText) enigma `shouldBe`
            map (map reEnum) [[B,A,E,F,C,D],[D,F,E,A,C,B]]

    context "given an arbitrary enigma" $ modifyMaxSuccess (const 100) $ do
        let arbitraryCartridge = Cartridge <$> vectorOf 3 (elements allRotors26)
                                        <*> elements allReflectors26
                                        <*> vectorOf 3 (chooseEnum (minBound,maxBound-1))
        let arbitraryPlugboard = elements allPlugboards26 :: Gen (Plugboard EnglishLetter)
        let arbitraryEnigmaState = Enigma <$> arbitraryPlugboard <*> arbitraryCartridge :: Gen (EnigmaState EnglishLetter)
        let arbitraryText = listOf (listOf (chooseEnum (minBound, maxBound-1)))
        it "should reencrypt all letters undeterministically in same order" $
            forAll arbitraryEnigmaState $ \c@(Enigma _ (Cartridge _ _ ps)) ->
            forAll arbitraryText $ \p ->
                evalState (
                    encryptTraversableOfMonads (
                        evalState (
                            encryptTraversableOfMonads
                            p)
                        c))
                    c
                `shouldBe`
                    p

reencryptionProp :: (Show a, Cipher c, Cipherable a) => c a -> a -> Property
reencryptionProp c p = decrypt c (encrypt c p) === p

main :: IO ()
main =
    hspec $ do
        let n = 4
        transformSpec
        wiringSpec (Bombe.Wiring.Wiring.initialize n :: MatrixWiringStandard)
        wiringSpec (Bombe.Wiring.Wiring.initialize n :: MatrixWiringCompressed)
        wiringSpec (Bombe.Wiring.Wiring.initialize n :: MatrixWiringLegacy)
        transitiveClosureSpec
        tracedEncryptionSpec
        cartridgeSpec
        enigmaSpec
        undeterministicEnigmaSpec

