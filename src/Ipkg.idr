module Ipkg

import Data.List
import Data.String
import System.File.ReadWrite
import Version
import Util


public export
record Ipkg where
    constructor MkIpkg
    name : String
    version : Maybe Version
    depends : List String
    modules : List String
    main : Maybe String
    exec : Maybe String
    passthru : List (String, String)


Show Ipkg where
    show p = let depends = if p.depends == [] then Nothing else Just "depends = \{concat $ intersperse ", " p.depends}"
                 version = p.version <&> \v => "version = \{show v}"
                 mains = p.main <&> \s => "main = \{s}"
                 exec = p.exec <&> \s => "executable = \{s}"
                 passthru = fastUnlines $ map (\(k, s) => "\{k} = \"\{s}\"" ) p.passthru
      in """
         package \{p.name}
         sourcedir = "src"
         modules = \{concat $ intersperse ", " p.modules}
         """
         ++ "\n" ++ (unlines $ catMaybes [version, depends, mains, exec])
         ++ passthru


public export
writeIpkg : Ipkg -> String -> M ()
writeIpkg ipkg dest = do
    Right () <- writeFile dest (show ipkg) | Left err => mErr $ show err
    pure ()
