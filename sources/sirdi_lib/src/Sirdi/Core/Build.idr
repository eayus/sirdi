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
import Idris.Version
import Idris.Env

import Util.Ipkg
import System


public export
data BuildError : Type where
    CompileError : Core.Error -> BuildError


public export
record BuildTree (ident : Identifier) where
    constructor MkBuildTree
    pkg : Package Built ident
    depTrees : All BuildTree pkg.description.dependencies


recursiveDepHashes : All BuildTree pkg.description.dependencies -> List String
recursiveDepHashes builtDeps = concat $ mapAll f builtDeps
    where
        f : BuildTree ident -> List String
        f bd = bd.pkg.identHash' :: recursiveDepHashes bd.depTrees


removeIdrSuffix : String -> String
removeIdrSuffix = pack . go . unpack
    where
        go : List Char -> List Char
        go = reverse . drop 4 . reverse

replaceSlash : String -> String
replaceSlash = pack . go . unpack
    where
        replace : Char -> Char
        replace '/' = '.'
        replace c   = c

        go : List Char -> List Char
        go = map replace


doBuild : (pkg : Package Fetched ident) -> All BuildTree pkg.description.dependencies -> IOEither BuildError ()
doBuild pkg deps = do
    let hash = pkg.identHash'
    let dir = sirdiDir /> hash
    let srcDir = dir /> "src"
    let desc = pkg.description
    let ipkgPath = dir /> hash <.> "ipkg"


    Just findOutput <- run "find \{show $ srcDir} -type f -name \"*.idr\" -exec realpath --relative-to \{show $ srcDir} {} \\\;"
        | Nothing => die "Failed to execute 'find' to find modules"

    let modulePaths = lines findOutput

    let modules = map (replaceSlash . removeIdrSuffix) modulePaths


    -- How do we want to handle recursive dependencies.
    --let depends = mapAll (.identHash') deps
    let depends = recursiveDepHashes deps

    dieOnLeft $ createDir $ show $ dir /> "depends"

    ignore $ traverse (\depHash => do
        let depDir = ((sirdiDir /> depHash) /> "build") /> "ttc"
        let target = (dir /> "depends") /> depHash

        --system "ln -s \{show depDir} \{show target}"
        system "cp -r \{show depDir} \{show target}"
        ) depends

    let ipkg = MkIpkg {
        name = hash,
        depends = depends,
        modules = modules,
        main = desc.main,
        exec = desc.main $> "main" }

    dieOnLeft $ writeIpkg ipkg $ show ipkgPath

    ignore $ system $ "idris2 --build " ++ show ipkgPath

    ignore $ system $ "rm -r " ++ (show $ dir /> "depends")


export
build : Initialised
     => (pkg : Package Fetched ident)
     -> All BuildTree pkg.description.dependencies
     -> IOEither BuildError (Package Built ident)
build pkg deps = doBuild pkg deps $> coerceState pkg

