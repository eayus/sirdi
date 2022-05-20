module Sirdi.Core.Util

import Data.List.Quantifiers


public export
allList : {xs : List a} -> {0 p : a -> Type} -> All p xs -> List (DPair a p)
allList []        = []
allList (p :: ps) = (_ ** p) :: allList ps
