module Lib
    ( someFunc
    ) where

import Parts ( donitz ) 
import Diagram ( drawEnigma, defaultShape )
import Diagrams.Backend.SVG.CmdLine ( mainWith )
import Control.Monad.Reader (runReader)
import Bombe.Tracker (doTrackSpecsOf)

someFunc :: IO ()
someFunc = do
    doTrackSpecsOf donitz
    mainWith $ runReader (drawEnigma donitz 0) defaultShape
    --mainWith $ maybe mempty (\x -> runReader (drawEnigma x 0) defaultShape) (Just donitz)