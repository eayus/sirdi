module Sirdi.Core

import public System.Path
import public Util.Pred

import Data.Hashable


public export
data Identifier : Type where
    Local : (path : Path) -> Identifier
    Git : (url : String) -> (commit : String) -> (path : Path) -> Identifier


export
record Description (ident : Identifier) where
    constructor MkDescription
    main' : Maybe String
    deps' : List Identifier


-- TODO: Remove the indices of this, and use a separate proof..
export
record Store (fetched : Pred Identifier) (built : Pred Identifier) where
    constructor MkStore


export
(.main) : Description ident -> Maybe String
(.main) = (.main')


export
(.deps) : Description ident -> List Identifier
(.deps) = (.deps')


sirdiDir : Path
sirdiDir = parse ".sirdi"


configName : String
configName = "sirdi.toml"


(.hash) : Identifier -> String
(.hash) (Local path)          = "local" ++ (show $ hash $ show path)
(.hash) (Git url commit path) = ?identHash_rhs_2
