module Sirdi.Core.Build

import Sirdi.Core
import Sirdi.Core.Paths


export
build : (1 store : Store)
     -> (ident : Identifier)
     -> (fetched : Fetched store ident)
     -> (tree : DepTree store ident)
     -> (depsBuilt : DepsBuilt tree)
     -> IO Store
build MkStore ident _ tree _ = do
    

    pure MkStore
