module DepTree

import Config
import Data.String


public export
record Tree a where
    constructor Node
    val : a
    children : List (Tree a)


public export
DepTree : Type
DepTree = Tree Dependency


showDep : Dependency -> String
showDep dep = "\{dep.name} (\{showSrc dep.source})"
    where
        showSrc : Source -> String
        showSrc (Git x)   = x
        showSrc (Local x) = x


showTree : String -> DepTree -> String
showTree indent node =
    let subtrees = unlines $ map (showTree $ indent ++ " |  ") node.children in
        "\{indent} +- \{showDep node.val}\n\{subtrees}"


public export
Show DepTree where
  show = showTree ""