module Sirdi

import Sirdi.Core.Interface
import Util.All


Self : Identifier
Self = Local "."


data RecBuildError : Type where
    FetchErr : (ident : Identifier) -> FetchError ident -> RecBuildError
    BuildErr : BuildError -> RecBuildError


export
recBuild : SirdiCore =>
           Initialised =>
           (ident : Identifier) ->
           IOEither RecBuildError (Package Built ident)
recBuild ident = do
    pkg <- mapErr (FetchErr ident) (fetch ident)
    deps <- mapM recBuild pkg.description.dependencies
    mapErr BuildErr $ build pkg deps
