module Bombe.Bombe where

import Bombe.Tracker
import Rotor (Rotor(..))


{-
permutedPositions n =
            let
                helper []     = []
                helper (_:xs) = [0..(n-1)] : helper xs

                permuteHelper [] = [[]]
                permuteHelper (ls:lss) = (++) <$> map pure ls <*> permuteHelper lss
            in
                permuteHelper $ helper positions
-}

{-
--analyze a single enigma
analyze :: [Rotor l] -> Rotor l -> String -> [BombeSolution l] 
analyze rotors reflector crib = 
    do
        let n          = length crib
            engageTree = trackEngageTreeOfLength rotors reflector n
        position <- permutedPositions n
        let enigma = EnigmaState idPlugboard (Cartridge rotors reflector position)
        --engage enigma according to tree, 
        --for each engage, 
        -- -- encrypt entirety, 
        -- -- apply wiring
        --when hitting a leaf, check 
        --if valid store solution, otherwise backtrack
        analyzeEngage (EmptyWiring, []) engagetree
        
        
        
        []

isValidWiring :: Wiring -> Bool 

getValidSolutions :: Wiring -> EnigmaState l -> [BombeSolution l]

analyzeEngage :: Wiring -> 
                 EnigmaState l ->  
                 Tree Engages -> 
                 [BombeSolution l]
analyzeEngage wiring enigma Leaf = 
    if isValidWiring wiring 
        then getValidSolutions wiring e 
        else []
analyzeEngage wiring enigma (Forest branches) = 
    foldl 
        (\acc (engages, subtree) -> 
            let 
                engagedEnigma = engageEnigma engages enigma
                newWiring     = connectWires wiring engagedEnigma
            in
                acc ++ analyzeEngage newWiring engagedEnigma subtree             
            )
        []
        branches

-}
