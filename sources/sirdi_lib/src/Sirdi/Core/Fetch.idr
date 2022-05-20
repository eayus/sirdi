module Sirdi.Core.Fetch

import Sirdi.Core
import Util.IOEither

import System
import System.Directory


fetchLocal : String -> Path -> IO ()
fetchLocal hash path = do
    let pkgDir = sirdiDir /> hash
    let destDir = pkgDir /> "src"

    unless !(exists $ show pkgDir) (dieOnLeft $ createDir $ show pkgDir)

    ignore $ system "rm -rf \{show destDir}"

    ignore $ system "cp -r \{show path}/src \{show destDir}"


fetchGit : String -> String -> String -> Path -> IO ()
fetchGit hash url commit path = ?fetchGit_rhs


fetch' : String -> Identifier -> IO ()
fetch' hash (Local path)          = fetchLocal hash path
fetch' hash (Git url commit path) = fetchGit hash url commit path


export
fetch : (ident : Identifier)
     -> (1 store : Store fetched built)
     -> IO (Store (Add ident fetched) (Remove ident built))
fetch ident MkStore = fetch' ident.hash ident $> MkStore
