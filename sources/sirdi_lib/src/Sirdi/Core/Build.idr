module Sirdi.Core.Build

import Sirdi.Core
import Util.Files
import Util.Ipkg
import Util.All
import Util.IOEither

import Data.List
import Data.String
import Data.List.Quantifiers
import System
import System.Directory


public export
record DepTree (root : Identifier) where
    constructor DepNode
    desc : Description root
    children : All DepTree desc.deps


-- Some weird idris bug prevents this from being a record
public export
data RecBuilt : Pred Identifier -> Identifier -> Type where
    RecNode : built root -> (desc : Description root) -> All (RecBuilt built) desc.deps -> RecBuilt built root


depHashes : DepTree root -> List String
depHashes (DepNode desc children) = map (.hash) desc.deps ++ concat (mapAll depHashes children)


loadDep : Path -> String -> IO ()
loadDep dependsDir hash = do
    let ttcDir = ((sirdiDir /> hash) /> "build") /> "ttc"
    let targetDir = dependsDir /> hash

    ignore $ system "cp -r \{show ttcDir} \{show targetDir}"


build' : (ident : Identifier) -> DepTree ident -> IO ()
build' ident tree = do
    let packageDir = sirdiDir /> ident.hash
    let sourceDir  = packageDir /> "src"
    let dependsDir = packageDir /> "depends"
    let ipkgPath   = packageDir /> ident.hash <.> "ipkg"

    modules <- findModules sourceDir

    let depends = depHashes tree

    dieOnLeft $ createDir $ show dependsDir
    ignore $ traverse (loadDep dependsDir) depends


    let ipkg = MkIpkg {
        iname = ident.hash,
        idepends = depends,
        imodules = modules,
        imain = tree.desc.main,
        iexec = tree.desc.main $> "main" }

    dieOnLeft $ writeIpkg ipkg $ show ipkgPath

    ignore $ system $ "idris2 --build " ++ show ipkgPath

    ignore $ system $ "rm -r " ++ show dependsDir


export
build : (ident : Identifier)
     -> (tree : DepTree ident)
     -> (0 depsBuilt : All (RecBuilt built) tree.desc.deps)
     -> (0 isFetched : fetched ident)
     -> (1 store : Store fetched built)
     -> IO (Store fetched (Add ident built))
build ident tree _ _ MkStore = build' ident tree $> MkStore
