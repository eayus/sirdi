module Sirdi.Core.Build

import Sirdi.Core
import Sirdi.Core.Init
import Core.Context
import Data.List.Quantifiers
import Util.IOEither
import Util.All
import Idris.ModTree
import Compiler.Common
import Idris.Syntax
import Idris.REPL.Opts
import System.Path
import System.Directory


public export
data BuildError : Type where
    CompileError : Core.Error -> BuildError


public export
BuiltDepsFor : Package Fetched ident -> Type
BuiltDepsFor pkg = All (Package Built) (pkg.description.dependencies)




doBuildCore : Ref Ctxt Defs
           => Ref Syn SyntaxInfo
           => Ref ROpts REPLOpts
           => (pkg : Package Fetched ident) -> BuiltDepsFor pkg -> Core ()
doBuildCore pkg deps = do
    -- Set the source dir
    setSourceDir $ Just $ show $ sourcesDir /> pkg.identHash'

    -- Set the build dir
    setBuildDir $ show $ outputsDir /> pkg.identHash'

    -- Tell Idris where dependencies are
    _ <- traverseAll' (\dep => addExtraDir $ show $ outputsDir /> dep.identHash') deps

{-
let toBuild = maybe (map snd (modules pkg))
                        (\m => snd m :: map snd (modules pkg))
                                                (mainmod pkg)
                                                    buildAll toBuild-}

    errs <- buildAll ?h1

    ?doBuildCore_rhs


doBuildCore' : (pkg : Package Fetched ident) -> BuiltDepsFor pkg -> Core ()
doBuildCore' pkg deps = do
    c <- newRef Ctxt !(initDefs)
    s <- newRef Syn initSyntax
    o <- newRef ROpts (defaultOpts (Just "example-fname") (REPL NoneLvl) [])

    doBuildCore pkg deps


doBuild : (pkg : Package Fetched ident) -> BuiltDepsFor pkg -> IOEither BuildError ()
doBuild pkg deps = do
    let dir = outputsDir /> pkg.identHash'
    dieOnLeft $ createDir $ show dir

    mapErr CompileError $ coreToIOEither $ doBuildCore' pkg deps


export
build : Initialised =>
        (pkg : Package Fetched ident) ->
        BuiltDepsFor pkg ->
        IOEither BuildError (Package Built ident)
build pkg deps = doBuild pkg deps $> coerceState pkg

