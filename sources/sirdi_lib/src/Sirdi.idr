module Sirdi

import public Sirdi.Core
import public Sirdi.Core.Init
import public Sirdi.Core.Fetch
import public Sirdi.Core.Build
import public Sirdi.Core.Config
import public Sirdi.Core.New
import public Util.IOEither
import System.Path
import Util.All


public export
Self : Identifier
Self = Local $ parse "."


public export
data RecBuildError : Type where
    FetchErr : (ident : Identifier) -> FetchError ident -> RecBuildError
    BuildErr : BuildError -> RecBuildError


export
recBuild : Initialised =>
           (ident : Identifier) ->
           IOEither RecBuildError (Package Built ident)
recBuild ident = do
    pkg <- mapErr (FetchErr ident) (fetch ident)
    deps <- mapM recBuild pkg.description.dependencies
    mapErr BuildErr $ build pkg deps
