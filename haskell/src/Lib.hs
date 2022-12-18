module Lib
    ( someFunc
    ) where

import Parts ( donitz, simple4, simple6)
import Enigma (stepEnigma)
import Diagram ( drawEnigma, defaultShape )
import Diagrams.Backend.SVG.CmdLine ( mainWith )
import Control.Monad.Reader (runReader)
import Data.List (iterate')
import Diagrams (iterateN)
someFunc :: IO ()
someFunc = do
    mainWith $ runReader (drawEnigma donitz 0) defaultShape
    --mainWith $ maybe mempty (\x -> runReader (drawEnigma x 0) defaultShape) (Just donitz)