module Language (
Letter(..), 
readLetters, 
safelyReadLetters,
shiftLetter,
stringToLanguage
) where

import Text.Read (readMaybe)
-----------------------------------------------------------------------
--                          LANGUAGE                                 --
-----------------------------------------------------------------------
--class HasLanguage a where
--    getLanguage :: (Language l) => a l -> l

--instance (Language l) => HasLanguage (a l) where 
--    getLanguage = 



data Letter = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving(Eq,Enum,Show,Read,Ord)
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

stringToLanguage :: (Show e, Enum e) => String -> [e]
stringToLanguage xs = 
    let 
        ans = fmap (\x -> toEnum (fromEnum x - fromEnum 'A')) xs
    in ans