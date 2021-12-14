module DepTree

import Config
import Data.String


public export
record Tree a where
    constructor Node
    val : a
    children : List (Tree a)


export
treeToList : Tree a -> List a
treeToList node = node.val :: concatMap treeToList node.children


public export
DepTree : Type
DepTree = Tree (Package Unspecified)


showDep : Package Unspecified -> String
showDep dep = "\{dep.name} (\{showSrc dep.source})"
    where
        showSrc : Source sk -> String
        showSrc (Git x _)   = x
        showSrc (Local x) = x
        showSrc Legacy = "legacy"


showTree : String -> DepTree -> String
showTree indent node =
    let subtrees = fastConcat $ map (showTree $ indent ++ " |  ") node.children in
        "\{indent} +- \{showDep node.val}\n\{subtrees}"


public export covering
Show DepTree where
  show = showTree ""


