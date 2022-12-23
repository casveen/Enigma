{-# LANGUAGE DeriveFoldable #-}

module Bombe.Tracker where

import Enigma ( step, getRotorPosition, EnigmaState (Enigma) )
import Control.Monad.State.Strict (runState)
import Debug.Trace (trace)
import Cartridge (Cartridge(..))
import Cipher (Cipher(..))
import Data.List (nub, groupBy, partition)
import Control.Monad.Writer.Lazy (WriterT, Writer, tell)
--import Control.Monad.Identity (Identity)
--import Control.Monad.Trans.Class (lift)
--import Control.Monad.List (ListT, select)
--import Data.Tree
import Control.Monad.Amb
import Plugboard (mkPlugboard)
import Rotor (Rotor(..))
--import qualified List.Transformer as LT 

type Position = [Int]
--type List     = ListT Identity
--type Progress a = WriterT String (Amb a) a
--reportProgress :: Int -> Int -> Progress ()
--reportProgress p tot = tell $ show ((p*100)/tot) ++ "%"


runStateForever :: (Show b) => (a -> (b, a)) -> a -> [b]
runStateForever trans init =
    let
        val = trans init
        next = snd val
        out = fst val
    in
        out : runStateForever trans next

takeWhileNoRepeats :: (Eq a) => [a] -> [a]
takeWhileNoRepeats =
    let
        takeWhileNoRepeatsHelper acc [] = acc
        takeWhileNoRepeatsHelper acc (x:xs) =
            if x `elem` acc
                then acc
                else takeWhileNoRepeatsHelper (x:acc) xs
    in
        takeWhileNoRepeatsHelper []

trackManually :: EnigmaState l -> [[Int]]
trackManually st =
    let
        stepOnce = runState (do step; getRotorPosition)
        stepForever = runStateForever stepOnce st --has to be taken out of monad since this state monad is strict!
    in
        takeWhileNoRepeats stepForever


data Tree a = Leaf | Forest [(a, Tree a)] deriving(Eq, Show, Foldable)
type Engages    = [Bool]
type EngagePath = [Engages]
type EngageTree = Tree Engages



groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go [] where
  go acc comp [] = acc
  go acc comp (h:t) =
    let (hs, nohs) = partition (comp h) t
    in go ((h:hs):acc) comp nohs

showBools :: [Bool] -> String
showBools []     = ""
showBools (x:xs) = (if x == True then 'T' else 'F'):showBools xs

--trackEngageTreeOfLength :: EnigmaState l -> Int -> EngageTree
--trackEngageTreeOfLength st@(Enigma _ c@(Cartridge rotors reflector positions)) s =

trackEngageTreeOfLength :: Ord e => [Rotor e] -> Rotor e -> Int -> EngageTree
trackEngageTreeOfLength rotors reflector s = 
    let
        stepOnce            = runState (do step; getRotorPosition)
        stepForever         = runStateForever stepOnce             --has to be taken out of monad since this state monad is strict!
        --trackSpecific :: EnigmaState l -> [[Int]]
        trackSpecific    ss = take s . stepForever $ ss
        toEngages []        = []
        toEngages [x]       = []
        toEngages (cr:nr:r) = zipWith (/=) cr nr : toEngages (nr:r) --true at pos if changed
        n                   = letters reflector

        permutedPositions   =
            let
                helper []     = []
                helper (_:xs) = [0..(n-1)] : helper xs

                permuteHelper [] = [[]]
                permuteHelper (ls:lss) = (++) <$> map pure ls <*> permuteHelper lss
            in
                permuteHelper $ helper rotors

        workWithAllPositions :: [EngagePath] --WriterT String (Amb EngagePath)

        workWithAllPositions = do --we are in the progress monad, a writer on a list
            let allPositions   = permutedPositions
            arbitraryPosition <- allPositions
            let newCartridge   = Cartridge rotors reflector arbitraryPosition --an arbitrary cartridge
            let newEnigma      = Enigma (mkPlugboard []) newCartridge --ord comes from here...
            let allEngages     = toEngages . trackSpecific $ newEnigma
            return allEngages

        convergeTracksToTree :: [EngagePath] -> EngageTree
        convergeTracksToTree tracks =
            let
                partitionPaths = groupBy2 (\p1 p2 -> head p1 == head p2) tracks
                uniqueAndRest :: [ (Engages, [EngagePath]) ]
                uniqueAndRest  = map (\paths -> (head . head $ paths, map tail paths)) partitionPaths
            in 
                if null (head tracks)
                    then Leaf
                    else Forest $ map (\(u, r) -> (u, convergeTracksToTree r)) uniqueAndRest
    in
        convergeTracksToTree workWithAllPositions


drawTree :: Tree Engages -> Int -> String 
drawTree (Leaf) _ = "\n"
drawTree (Forest trees) d = 
    foldl 
        (\acc (branch, tree) ->
            acc ++ 
            (replicate d ' ') ++
            showBools branch ++ 
            (if (tree == Leaf) then "\n"
            else "->\n" ++ drawTree tree (d+2+(length (showBools branch))))) 
        "" 
        trees

--engageTreeToProperTree :: EngageTree -> Tree Engages
--engageTreeToProperTree tree = unfoldTree () [False] 

countLeaves (Forest trees) = foldl (\acc (_, tree) -> acc+(countLeaves tree)) 0 trees
countLeaves (Leaf) = 1

--doTrackSpecsOf :: EnigmaState l-> IO ()
doTrackSpecsOf :: Ord e => EnigmaState e -> IO ()
doTrackSpecsOf  e@(Enigma _ (Cartridge rotors reflector positions))  = do
    print $ allLeaveCountsOfEngagetre 5 30 e
    let tracks = trackManually e
    --print $ tracks
    putStrLn $ "Orbit length: " ++ show (length tracks)

allLeaveCountsOfEngagetre f t e@(Enigma _ (Cartridge rotors reflector positions)) = 
    if f<t 
        then countLeaves (trackEngageTreeOfLength rotors reflector f) : allLeaveCountsOfEngagetre (f+1) t e
        else []
