module OperatorBool where

import Parser

less :: [SExpr] -> Either String [SExpr]
less (ExprAtom (AtomNum x):ExprAtom (AtomNum y):ExprAtom (AtomNum z):xs) =
    Left "too many arg"
less [ExprAtom (AtomNum x), ExprAtom (AtomNum y)] =
    Right [ExprAtom (AtomBool (x < y))]
less _ = Left "less"

more :: [SExpr] -> Either String [SExpr]
more (ExprAtom (AtomNum x):ExprAtom (AtomNum y):ExprAtom (AtomNum z):xs) =
    Left "too many arg"
more [ExprAtom (AtomNum x), ExprAtom (AtomNum y)] =
    Right [ExprAtom (AtomBool (x > y))]
more _ = Left "more"

equa :: [SExpr] -> Either String [SExpr]
equa (ExprAtom (AtomNum x):ExprAtom (AtomNum y):ExprAtom (AtomNum z):xs) =
    Left "too many arg"
equa [ExprAtom (AtomNum x), ExprAtom (AtomNum y)] =
    Right [ExprAtom (AtomBool (x == y))]
equa [ExprAtom (AtomBool x), ExprAtom (AtomBool y)] =
    Right [ExprAtom (AtomBool (x == y))]
equa _ = Right [ExprAtom (AtomBool False)]

lessequa :: [SExpr] -> Either String [SExpr]
lessequa (ExprAtom (AtomNum x):ExprAtom (AtomNum y):ExprAtom (AtomNum z):xs) =
    Left "too many arg"
lessequa expr = case less expr of
    Right res@[ExprAtom (AtomBool True)] -> Right res
    Right res@[ExprAtom (AtomBool False)] -> case equa expr of
        Right res' -> Right res'
        Left error -> Left error
    Right _ -> Left "lessequa"
    Left error -> Left error

morequa :: [SExpr] -> Either String [SExpr]
morequa (ExprAtom (AtomNum x):ExprAtom (AtomNum y):ExprAtom (AtomNum z):xs) =
    Left "too many arg"
morequa expr = case more expr of
    Right res@[ExprAtom (AtomBool True)] -> Right res
    Right res@[ExprAtom (AtomBool False)] -> case equa expr of
        Right res' -> Right res'
        Left error -> Left error
    Right _ -> Left "morequa"
    Left error -> Left error

atom :: [SExpr] -> Either String [SExpr]
atom (ExprAtom (AtomNum x):xs) = 
    if null xs
    then Right [ExprAtom (AtomBool True)]
    else Left "too many args"
atom (ExprAtom (AtomStr x):xs) = 
    if null xs
    then Right [ExprAtom (AtomBool True)]
    else Left "too many args"
atom (ExprAtom (AtomBool x):xs) = 
    if null xs
    then Right [ExprAtom (AtomBool True)]
    else Left "too many args"
atom (ExprAtom AtomNil:xs) = 
    if null xs
    then Right [ExprAtom (AtomBool True)]
    else Left "too many args"
atom _ = Right [ExprAtom (AtomBool False)]