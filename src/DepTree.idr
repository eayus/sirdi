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
toList : Tree a -> List a
toList (Node val children) = val :: (children >>= toList)


public export
DepTree : Type
DepTree = Tree Package


showDep : Package -> String
showDep dep = "\{dep.name} (\{showSrc dep.source})"
    where
        showSrc : Source -> String
        showSrc (Git x)   = x
        showSrc (Local x) = x
        showSrc Legacy = "legacy"


showTree : String -> DepTree -> String
showTree indent node =
    let subtrees = fastConcat $ map (showTree $ indent ++ " |  ") node.children in
        "\{indent} +- \{showDep node.val}\n\{subtrees}"


public export covering
Show DepTree where
  show = showTree ""


