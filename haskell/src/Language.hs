{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language (
Letter,
LetterOrdinal(..),
EnglishLetter,
Letter4,
Letter6,
readLetters,
safelyReadLetters,
shiftLetter,
stringToLanguage
) where

import Text.Read (readMaybe)
import Data.Mod.Word ( Mod )
import Cipher (Cipherable)
import Data.TypeLits (KnownNat)
import Test.QuickCheck (Arbitrary, chooseEnum)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

-----------------------------------------------------------------------
--                          LANGUAGE                                 --
-----------------------------------------------------------------------
data LetterOrdinal = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Eq,Enum,Show,Read,Ord, Bounded)
instance Num LetterOrdinal where

    (+) l r = toEnum $ mod (fromEnum l + fromEnum r) maxBound
    (-) l r = toEnum $ mod (fromEnum l - fromEnum r) maxBound
    (*) l r = toEnum $ mod (fromEnum l * fromEnum r) maxBound
    abs = id
    signum = id
    fromInteger = toEnum . fromIntegral

type Letter m = Mod m

type EnglishLetter = Letter 26

type Letter4 = Letter 4

type Letter6 = Letter 6

instance (KnownNat m) => Cipherable (Mod m)
instance Cipherable LetterOrdinal

instance Arbitrary LetterOrdinal where
  arbitrary = chooseEnum(minBound, maxBound)


instance (KnownNat m) => Arbitrary (Mod m) where
  arbitrary = chooseEnum(minBound, maxBound)

safelyReadLetters :: Enum a => String -> Maybe [a]
safelyReadLetters (s:str) = do
    l <- fmap (toEnum . fromEnum) (readMaybe [s] :: Maybe LetterOrdinal)
    ls <- safelyReadLetters str
    return $ l:ls
safelyReadLetters [] = Just []

readLetters ::  (Enum e) => String -> [e]
readLetters str =
    let
        readAsOrdinals = map (\c -> read (c:"")) str :: [LetterOrdinal]
        reEnum :: (Enum e) => LetterOrdinal -> e
        reEnum = toEnum . fromEnum
    in
        map reEnum readAsOrdinals

shiftLetter :: (Enum a) => a -> Int -> Int -> a
shiftLetter l p n = toEnum $ mod (fromEnum l + p) n

stringToLanguage :: (Enum e) => String -> [e]
stringToLanguage xs =
    let
        ans = fmap (\x -> toEnum (fromEnum x - fromEnum 'A')) xs
    in ans