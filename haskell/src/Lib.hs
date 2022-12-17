module Lib
    ( someFunc
    ) where

import Parts ( donitz )
import Diagram ( drawEnigma, defaultShape )
import Diagrams.Backend.SVG.CmdLine ( mainWith )
import Control.Monad.Reader (runReader)
someFunc :: IO ()
someFunc = do
    mainWith $ maybe mempty (\x -> runReader (drawEnigma x 0) defaultShape) (Just donitz)