module Sirdi.Core.Interface

import public Data.List.Quantifiers
import public Util.IOEither
import public Language.TOML   -- Export TOML.Error
import public Core.Context    -- Export Core.Error


public export
data Identifier : Type where
    Local : (path : String) -> Identifier
    Git : (url : String) -> (commit : String) -> (path : String) -> Identifier


public export
record Description where
    constructor MkDescription
    dependencies : List Identifier


public export
data PackageState : Type where
    Fetched : PackageState
    Built : PackageState


namespace InitError
    public export
    data InitError : Type where
        NoConfigFile : InitError


public export
data ConfigError : Type where
    TOMLError : TOML.Error -> ConfigError
    ValidateError : String -> ConfigError


public export
data FetchError : Identifier -> Type where
    BadGitRepo : (badURL : String) -> FetchError (Git badURL commit path)
    BadGitCommit : (badCommit : String) -> FetchError (Git url badCommit path)
    BadGitPath : (badPath : String) -> FetchError (Git url commit badPath)

    BadLocalPath : (badPath : String) -> FetchError (Local badPath)

    NoConfigFile : FetchError ident
    BadConfig : ConfigError -> FetchError ident


public export
data BuildError : Type where
    CompileError : Core.Error -> BuildError


public export
interface SirdiCore where
    Initialised : Type

    Package : PackageState -> Identifier -> Type

    init : IOEither InitError Initialised

    (.description) : Package state ident -> Description

    fetch : Initialised => (ident : Identifier) -> IOEither (FetchError ident) (Package Fetched ident)

    build : Initialised =>
            (pkg : Package Fetched ident) ->
            -- Working around https://github.com/idris-lang/Idris2/issues/2246
            All (Package Built) (((.description) {state = Fetched} {ident = ident} pkg).dependencies) ->
            IOEither BuildError (Package Built ident)
