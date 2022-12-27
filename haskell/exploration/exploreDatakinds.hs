{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

import Data.TypeNums
import Numeric.LinearAlgebra.Data
import GHC.Num.Natural

type ModTwo = Numeric.LinearAlgebra.Data.Mod 2 Z 
type M2 = Matrix ModTwo

m = (3><3) [1,2,3,4,5,6,7,8,9] :: M2

--instance KnownNat Two where 
