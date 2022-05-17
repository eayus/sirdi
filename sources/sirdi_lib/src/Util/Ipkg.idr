module Util.Ipkg

import Data.List
import Data.String
import System.File.ReadWrite


public export
record Ipkg where
    constructor MkIpkg
    name : String
    depends : List String
    modules : List String
    main : Maybe String
    exec : Maybe String


Show Ipkg where
    show p = let depends = if p.depends == [] then Nothing else Just "depends = \{concat $ intersperse ", " p.depends}"
                 mains = p.main <&> \s => "main = \{s}"
                 exec = p.exec <&> \s => "executable = \{s}"
      in """
         package \{p.name}
         sourcedir = "src"
         modules = \{concat $ intersperse ", " p.modules}
         """
         ++ "\n" ++ (unlines $ catMaybes [depends, mains, exec])


public export
writeIpkg : HasIO io => Ipkg -> String -> io (Either FileError ())
writeIpkg ipkg dest = writeFile dest $ show ipkg
