module Lib
    ( someFunc
    ) where

import Parts ( donitz )
import Diagram ( drawEnigma )
import Diagrams.Backend.SVG.CmdLine ( mainWith )
someFunc :: IO ()
someFunc = do
    mainWith $ maybe mempty (`drawEnigma` 0) (Just donitz)