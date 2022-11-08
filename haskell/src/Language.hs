module Language (
Language(..), 
HasLanguage(..),
English(..),
Minimal(..), 
Letter(..), 
readLetters, 
safelyReadLetters,
shiftLetter 
) where

import Text.Read (readMaybe)

-----------------------------------------------------------------------
--                          LANGUAGE                                 --
-----------------------------------------------------------------------
class (Show l) => Language l where
    letters :: l -> Int
class HasLanguage a where
    getLanguage :: (Language l) => a l -> l

data English = English deriving(Eq, Show, Read)
data Minimal = Minimal deriving(Eq, Show, Read)
instance Language English where
    letters _ = 26
instance Language Minimal where
    letters _ = 4

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Eq,Enum,Show,Read)
type Letters = [Letter]

safelyReadLetters :: String -> Maybe Letters
safelyReadLetters (s:str) = do
    l <- readMaybe [s]
    ls <- safelyReadLetters str
    return $ l:ls
safelyReadLetters [] = Just []

readLetters :: String -> Letters
readLetters = map (\l -> read [l])

shiftLetter :: (Enum a) => a -> Int -> Int -> a
shiftLetter l p n = toEnum $ mod (fromEnum l + p) n

--monadicShiftLetter :: (Monad m, Enum a) => m a -> Int -> Int -> m a
--monadicShiftLetter l p n = (\x -> toEnum $ mod (fromEnum x + p) n) <$> l