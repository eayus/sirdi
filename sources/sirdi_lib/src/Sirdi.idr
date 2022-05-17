module Sirdi

import public Sirdi.Core
import public Sirdi.Core.Init
import public Sirdi.Core.Fetch
import public Sirdi.Core.Build
import public Sirdi.Core.Config
import public Sirdi.Core.New
import public Sirdi.Core.Run
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
buildTree : Initialised
         => (ident : Identifier)
         -> IOEither RecBuildError (BuildTree ident)
buildTree ident = do
    pkg <- mapErr (FetchErr ident) (fetch ident)
    deps <- mapM buildTree pkg.description.dependencies
    pkg <- mapErr BuildErr $ build pkg deps
    pure $ MkBuildTree pkg deps


export
recBuild : Initialised
        => (ident : Identifier)
        -> IOEither RecBuildError (Package Built ident)
recBuild ident = (.pkg) <$> buildTree ident
{-
recBuild ident = do
    pkg <- mapErr (FetchErr ident) (fetch ident)
    deps <- mapM recBuild pkg.description.dependencies
    mapErr BuildErr $ build pkg deps
-}


--export
--buildAndRun : Initialised => (ident : Identifier) -> IOEither RecBuildError Int
--buildAndRun ident = recBuild ident >>= run
