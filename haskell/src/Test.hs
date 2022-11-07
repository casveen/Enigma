module Test (
    safeHead,
    findValue, 
    noRepeats,
    isOnto,
    inverse,
    validRotor
) where


-----------------------------------------------
--              TESTING                      --
----------------------------------------------- 
--check validity
--helper functions
safeHead [] = Nothing
safeHead (x:xs) = Just x

--find :: (Eq a) => a -> [a] -> Maybe a
findValue xs i = isJust (findIndex xs i)
findIndex xs i = safeHead (dropWhile (/= i) xs)

noRepeats [] = True
noRepeats (x:xs) = not (findValue xs x) && noRepeats xs

--a transform is onto if each row has exactly one element(ie, the value 1)
isOnto (Transform m _) = det m /= 0

inverse (Transform t _) = Transform (tr t)

--validRotor :: Rotor -> Bool
validRotor (Rotor t no) = isOnto t &&
--                                    all (== 1) (map (\m) t) 
                                      noRepeats no &&
                                      all (< n) no
    where
        n = length no
validRotor (Reflector t no) = validRotor (Rotor t no)
--                                isSymmetricTransformation t