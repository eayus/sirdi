module Config


public export
data Location
    = Link String
    | Local String


public export
record Config where
    constructor MkConfig
    deps : List Location
    modules : List (String) -- Need a better type for this one


export
parseConfig : (dir : String) -> IO Config
parseConfig dir = pure example
    where
        example : Config
        example = MkConfig
          {
            deps = [ Link "https://github.com/ShinKage/idris2-sdl" ],
            modules = [ "SDL" ]
          }


public export
Eq Location where
    (Link x) == (Link y) = x == y
    (Local x) == (Local y) = x == y
    _ == _ = False


public export
Show Location where
    show (Link s) = "Link \{s}"
    show (Local s) = "Local \{s}"
