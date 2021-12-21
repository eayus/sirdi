module Package.Description

import Package.Identifier
import Package.Source
import Util
import Data.List


public export
record Description where
    constructor MkDescription
    name : String
    deps : List (Identifier MaybePinned)
    modules : List String -- Need a better type for module names maybe?
    main : Maybe String
    passthru : List (String, String)


export
emptyDescription : Identifier a -> Description
emptyDescription pkg = MkDescription pkg.name [] [] Nothing []


public export
MultiDescription : Type
MultiDescription = List Description


export
findSubDescription : String -> MultiDescription -> M Description
findSubDescription name multi =
    case find (\cfg => cfg.name == name) multi of
         Just c => pure c
         Nothing => mErr "Cannot find definition for package \{name} in config"


export
getSubDescription : Maybe String -> MultiDescription -> M Description
getSubDescription (Just name) multi = findSubDescription name multi
getSubDescription Nothing [ x ] = pure x
getSubDescription Nothing [] = mErr "Empty configuration"
getSubDescription Nothing _ = mErr "Need to specify which subconfig to build"
