||| Types for specifying where a package is located. This is different to
||| 'Package.Source', as a single source location can contain multiple
||| packages.
module Package.Identifier

import Package.Source
import Data.Hashable
import Util


||| The unique identifier for a package a package, consisting of the
||| package's name and its source location.
public export
record Identifier (pk : PinKind) where
    constructor MkPkg

    ||| The name of the package, used to distinguish between the several
    ||| different packages that could be located at the same source.
    name : String

    ||| The location of the source files.
    source : Source pk


||| Get the pinned version of a package identifier.
export
pinIdentifier : Identifier MaybePinned -> M (Identifier IsPinned)
pinIdentifier ident = MkPkg ident.name <$> pinSource ident.source


||| Get the textual version of a package identifier. The returned string can
||| be safely passed to the filesystem.
export
(.asString) : Identifier IsPinned -> String
(.asString) pkg = "\{pkg.name}\{hashSource pkg.source}"
