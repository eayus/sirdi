module Ipkg

import Data.List
import System.File.ReadWrite
import Util


public export
record Ipkg where
    constructor MkIpkg
    name : String
    depends : List String
    modules : List String


Show Ipkg where
    show p = let depends = if p.depends == [] then "" else "depends = \{concat $ intersperse ", " p.depends}" in """
package \{p.name}
sourcedir = "src"
modules = \{concat $ intersperse ", " p.modules}
\{depends}
"""


public export
writeIpkg : Ipkg -> String -> M ()
writeIpkg ipkg dest = do
    Right () <- mIO $ writeFile dest (show ipkg) | Left err => mErr $ show err
    pure ()
