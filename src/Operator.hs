module Operator where

import Parser

add :: [SExpr] -> Int -> Either String [SExpr]
add [] a = Right [ExprAtom (AtomNum a)]
add (ExprAtom (AtomNum x):xs) a = add xs (x + a)
add _ _ = Left "add"

sub :: [SExpr] -> Int -> Either String [SExpr]
sub [] a = Right [ExprAtom (AtomNum a)]
sub (ExprAtom (AtomNum x):xs) a = sub xs (a - x)
sub _ _ = Left "sub"

mul :: [SExpr] -> Int -> Either String [SExpr]
mul [] a = Right [ExprAtom (AtomNum a)]
mul (ExprAtom (AtomNum x):xs) a = mul xs (x * a)
mul _ _ = Left "mul"

divi :: [SExpr] -> Either String [SExpr]
divi (ExprAtom (AtomNum x):ExprAtom (AtomNum 0):xs) = Left "div by 0"
divi (ExprAtom (AtomNum x):ExprAtom (AtomNum y):ExprAtom (AtomNum z):xs) =
    Left "too many arg"
divi (ExprAtom (AtomNum x):ExprAtom (AtomNum y):xs) =
    Right [ExprAtom (AtomNum (x `div` y))]
divi _ = Left "div"

modu :: [SExpr] -> Either String [SExpr]
modu (ExprAtom (AtomNum x):ExprAtom (AtomNum 0):xs) = Left "div by 0"
modu (ExprAtom (AtomNum x):ExprAtom (AtomNum y):ExprAtom (AtomNum z):xs) =
    Left "too many arg"
modu [ExprAtom (AtomNum x), ExprAtom (AtomNum y)] =
    Right [ExprAtom (AtomNum (x `mod` y))]
modu _ = Left "mod"

myabs :: [SExpr] -> Either String [SExpr]
myabs [ExprAtom (AtomNum x), ExprAtom (AtomNum y)] = Left "too many arg"
myabs [ExprAtom (AtomNum x)] = Right [ExprAtom (AtomNum (abs x))]
myabs [ExprAtom _] = Left "abs"