module Ipkg


public export
record Ipkg where
    constructor MkIpkg
    name : String
    depends : List String
    modules : List String


public export
writeIpkg : Ipkg -> String -> IO ()
