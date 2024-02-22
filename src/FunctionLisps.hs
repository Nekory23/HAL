{-# LANGUAGE BlockArguments #-}
module FunctionLisps where

import Parser

quote :: [SExpr] -> Either String [SExpr]
quote [] = Left "quote"
quote expr = Right expr

constructList :: Atom -> Atom -> [SExpr]
constructList x y = [ExprList [ExprAtom x, ExprAtom y]]

cons :: [SExpr] -> Either String [SExpr]
cons (ExprAtom x:ExprAtom y:xs) = 
    Right $ constructList x y
cons _ = Left "error"

getFirst :: [SExpr] -> [SExpr]
getFirst [] = []
getFirst (ExprList x:xs) = x
getFirst (ExprAtom x:xs) = [ExprAtom x]

getEnd :: [SExpr] -> [SExpr]
getEnd [] = []
getEnd (ExprList x:xs) = xs
getEnd (ExprAtom x:xs) = xs

car :: [SExpr] -> Either String [SExpr]
car [] = Left "error car"
car (ExprAtom (AtomStr "quote"):xs) = car xs
car (ExprAtom x:xs) = Left "error car"
car (ExprList x:xs) = Right (getFirst x)

cdr :: [SExpr] -> Either String [SExpr]
cdr [] = Left "error cdr"
cdr (ExprAtom (AtomStr "quote"):xs) = cdr xs
cdr (ExprAtom x:xs) = Left "error cdr"
cdr (ExprList x:xs) = Right (getEnd x)

lengths :: [SExpr] -> Either String [SExpr]
lengths [] = Left "no list"
lengths (ExprAtom (AtomStr "quote"):xs) = lengths xs
lengths (ExprList x:xs) = Right [ExprAtom (AtomNum (length x))]
lengths (ExprAtom x:xs) = Left "arg is not a list"

nulls :: [SExpr] -> Either String [SExpr]
nulls [] = Left "no args"
nulls (ExprList x:xs) =
    if null xs
    then Right [ExprAtom (AtomBool False)]
    else Left "too many args"
nulls (ExprAtom (AtomStr "quote"):ExprAtom AtomNil:xs) =
    if null xs
    then Right [ExprAtom (AtomBool True)]
    else Left "too many args"
nulls (ExprAtom (AtomStr "quote"):xs) =
    if length xs == 1
    then Right [ExprAtom (AtomBool False)]
    else Left "too many args"
nulls (ExprAtom x:xs) =
    if null xs
    then Right [ExprAtom (AtomBool False)]
    else Left "too many args"