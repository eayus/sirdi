module Sirdi.Core.Fetch

import Sirdi.Core
import Sirdi.Core.Paths

import System


public export
data FetchError : Identifier -> Type where
    CopyError : (error : String) -> FetchError (Local path)


export
fetch : (ident : Identifier) -> (1 store : Store) -> IO (Either (FetchError ident) Store)
fetch ident@(Local path) MkStore = do
    let src    = path /> "src"
    let target = pkgDir ident /> "src"

    (_, 0) <- run "cp -r \{show src} \{show target}"
        | (output, _) => pure $ Left $ CopyError output

    pure $ Right MkStore
