module Sirdi.Core.Paths

import Sirdi.Core

import Data.Hashable


export
sirdiDir : Path
sirdiDir = parse ".sirdi"


export
pkgName : Identifier -> String
pkgName (Local path) = "local" ++ (show $ hash $ show path)


export
pkgDir : Identifier -> Path
pkgDir x = sirdiDir /> pkgName x


export
cfgPath : Identifier -> Path
cfgPath x = pkgDir x /> "sirdi" <.> "toml"

