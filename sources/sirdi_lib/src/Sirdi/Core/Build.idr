module Sirdi.Core.Build

import Sirdi.Core
import Sirdi.Core.Init
import Core.Directory
import Core.Context
import Core.Metadata
import Core.UnifyState
import Data.String
import Data.List.Quantifiers
import Util.IOEither
import Util.All
import Util.Files
import Compiler.Common
import Idris.ModTree
import Idris.Syntax
import Idris.REPL.Opts
import Idris.SetOptions
import Idris.Package
import Idris.REPL
import System.Path
import System.Directory
import IdrisPaths


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
    let buildDir = show $ outputsDir /> pkg.identHash'
    setBuildDir buildDir

    -- Where to look for legacy stuff
    addPackageDir $ yprefix ++ "/idris2-0.5.1"

    -- Load prelude and base
    addPkgDir "base" anyBounds
    addPkgDir "prelude" anyBounds

    -- Tell Idris where dependencies are
    _ <- traverseAll' (\dep => addExtraDir $ show $ (outputsDir /> dep.identHash') /> "ttc") deps

    Just contents <- coreLift $ run "find \{show $ sourcesDir /> pkg.identHash'} -type f -name \"*.idr\""
        | Nothing => coreLift $ die "Failed to execute tree"

    let modules = lines contents

    errs <- buildAll modules

    coreLift $ putStrLn "Build errors:"
    coreLift $ print errs


    case pkg.description.main of
         Just mainMod => do
            modIdent <- ctxtPathToNS mainMod
            m <- newRef MD (initMetadata (PhysicalIdrSrc modIdent))
            u <- newRef UST initUState

            let mainName = NS (miAsNamespace modIdent) (UN $ Basic "main")
            let executableName = "main"

            ignore $ loadMainFile mainMod
            ignore $ compileExp (PRef replFC mainName) executableName
         Nothing => pure ()


doBuildCore' : (pkg : Package Fetched ident) -> BuiltDepsFor pkg -> Core ()
doBuildCore' pkg deps = do
    c <- newRef Ctxt !(initDefs)
    s <- newRef Syn initSyntax
    o <- newRef ROpts (defaultOpts (Just "example-fname") (REPL NoneLvl) [])

    doBuildCore pkg deps


doBuild : (pkg : Package Fetched ident) -> BuiltDepsFor pkg -> IOEither BuildError ()
doBuild pkg deps = do
    let dir = outputsDir /> pkg.identHash'

    unless !(exists $ show dir) (do
        dieOnLeft $ createDir $ show dir
        mapErr CompileError $ coreToIOEither $ doBuildCore' pkg deps)


export
build : Initialised =>
        (pkg : Package Fetched ident) ->
        BuiltDepsFor pkg ->
        IOEither BuildError (Package Built ident)
build pkg deps = doBuild pkg deps $> coerceState pkg

