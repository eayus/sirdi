module Config


public export
data Location
    = Link String
    --| Local String


public export
record Dependency where
    constructor MkDep
    loc : Location


public export
record Config where
    constructor MkConfig
    deps : List Dependency



export
parseConfig : (dir : String) -> IO Config
parseConfig dir = pure example
    where
        example : Config
        example = MkConfig [MkDep { loc = Link "https://github.com/ShinKage/idris2-sdl" }]



public export
Eq Location where
    (Link x) == (Link y) = x == y
