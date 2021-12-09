module DepTree

import Config
import Data.String


public export
record Tree a where
    constructor Node
    val : a
    children : List (Tree a)


public export
record DepInfo where
    constructor MkDepInfo
    pkgName : String
    loc : Location
    

public export
DepTree : Type
DepTree = Tree DepInfo


Show DepInfo where
    show x = "\{x.pkgName} (\{show x.loc})"



printTree : String -> DepTree -> String
printTree indent node =
    let subtrees = unlines $ map (printTree $ indent ++ " |  ") node.children in
        "\{indent} +- \{show node.val}\n\{subtrees}"


public export
Show DepTree where
    show = printTree ""
