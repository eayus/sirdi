module Sirdi.Core

import Sirdi.Core.Util

import public Data.List.Quantifiers
import public System.Path


public export
data Identifier : Type where
    Local : (path : Path) -> Identifier


export
record Info (ident : Identifier) where
    constructor MkInfo
    deps : List Identifier
    main : Maybe String
    mods : List String


export
record Store where
    constructor MkStore


export
record Fetched (store : Store) (ident : Identifier) where
    constructor IsFetched


export
record Built (store : Store) (ident : Identifier) where
    constructor IsBuilt


public export
record DepTree (store : Store) (root : Identifier) where
    constructor DepNode
    info     : Info root
    depTrees : All (DepTree store) info.deps


public export
DepsBuilt : DepTree store root -> Type
DepsBuilt (DepNode info depTrees) = (All (Built store) info.deps, All (\(_ ** tree) => DepsBuilt tree) (allList depTrees))
