module Sirdi.Core

import System.Path


public export
data Identifier : Type where
    Local : (path : Path) -> Identifier
    Git : (url : String) -> (commit : String) -> (path : Path) -> Identifier


public export
record Description where
    constructor MkDescription
    dependencies : List Identifier


public export
data PackageState : Type where
    Fetched : PackageState
    Built : PackageState


export
record Package (state : PackageState) (ident : Identifier) where
    constructor MkPackage
    identHash : String
    desc : Description


(.identHash') : Package state ident -> String
(.identHash') = (.identHash)


coerceState : Package state ident -> Package state' ident
coerceState (MkPackage identHash desc) = MkPackage identHash desc


export
(.description) : Package state ident -> Description
(.description) = (.desc)


-- Internal directories

sirdiDir : Path
sirdiDir = parse "." /> ".sirdi"

sourcesDir : Path
sourcesDir = sirdiDir /> "sources"

outputsDir : Path
outputsDir = sirdiDir /> "outputs"

configName : String
configName = "sirdi.toml"
