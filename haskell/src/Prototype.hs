module Prototype where
nonRepeatingEntiretyBuilder i (_:xs) =
            if i `elem` xs
                then i:nonRepeatingEntiretyBuilder (i+1) xs
                else nonRepeatingEntiretyBuilder (i+1) xs
nonRepeatingEntiretyBuilder _ [] = []