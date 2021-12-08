module Ipkg

import Data.List
import Data.String
import System.File.ReadWrite
import Util


public export
record Ipkg where
    constructor MkIpkg
    name : String
    depends : List String
    modules : List String
    main : Maybe String
    exec : Maybe String
    passthru : List (String, String)


Show Ipkg where
    show p = let depends = if p.depends == [] then "" else "depends = \{concat $ intersperse ", " p.depends}"
                 mains = case p.main of { Just s => "main = \{s}"; Nothing => "" }
                 exec = case p.exec of { Just s => "executable = \{s}"; Nothing => "" }
                 passthru = unlines $ map (\(k, s) => "\{k} = \{show s}" ) p.passthru
      in """
package \{p.name}
sourcedir = "src"
modules = \{concat $ intersperse ", " p.modules}
\{depends}
\{mains}
\{exec}
\{passthru}
"""


public export
writeIpkg : Ipkg -> String -> M ()
writeIpkg ipkg dest = do
    Right () <- mIO $ writeFile dest (show ipkg) | Left err => mErr $ show err
    pure ()
