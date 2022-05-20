module Util.Pred


public export
Pred : Type -> Type
Pred a = a -> Type


public export
data Add : a -> Pred a -> Pred a where
    This : Add e pred e
    That : {0 pred : Pred a} -> pred e -> Add x pred e


public export
data Remove : a -> Pred a -> Pred a where
    Other : {0 pred : Pred a} -> pred x -> Not (e = x) -> Remove e pred x
