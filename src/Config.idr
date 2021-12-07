module Config


public export
record Dependency where
    constructor MkDep
    link : String


public export
record Config where
    constructor MkConfig
    deps : List Dependency



export
parseConfig : IO Config
parseConfig = pure example
    where
        example : Config
        example = MkConfig [MkDep "https://github.com/ShinKage/idris2-sdl"]
