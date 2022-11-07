module Language (
Language(..), 
HasLanguage(..),
English(..),
Minimal(..), 
Letter(..), 
readLetters, 
shiftLetter,
monadicShiftLetter
) where

-----------------------------------------------------------------------
--                          LANGUAGE                                 --
-----------------------------------------------------------------------
class (Show a) => Language a where
    letters :: a -> Int
class HasLanguage a where
    getLanguage :: (Language l) => a l -> l

data English = English deriving(Eq, Show)
data Minimal = Minimal deriving(Eq, Show)
instance Language English where
    letters _ = 26
instance Language Minimal where
    letters _ = 4

data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Eq,Enum,Show,Read)

readLetters :: String -> [Letter]
readLetters = map (\ l -> read [l])

shiftLetter :: (Enum a) => a -> Int -> Int -> a
shiftLetter l p n = toEnum $ mod (fromEnum l + p) n

monadicShiftLetter :: (Monad m, Enum a) => m a -> Int -> Int -> m a
monadicShiftLetter l p n = (\x -> toEnum $ mod (fromEnum x + p) n) <$> l