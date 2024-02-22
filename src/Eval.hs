module Eval where

import Parser
import Operator
import OperatorBool
import FunctionLisps

evalList :: [SExpr] -> Env -> Either String [SExpr]
evalList ((ExprAtom (AtomStr "+")):xs) _ = case add xs 0 of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "-")):(ExprAtom (AtomNum y)):xs) _ = case sub xs y of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "*")):xs) _ = case mul xs 1 of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "div")):xs) _ = case divi xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "mod")):xs) _ = case modu xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "<")):xs) _ = case less xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr ">")):xs) _ = case more xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr ">=")):xs) _ = case morequa xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "<=")):xs) _ = case lessequa xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "eq?")):xs) _ = case equa xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "abs")):xs) _ = case myabs xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "atom?")):xs) _ = case atom xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "'")):xs) _ = case quote xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "quote")):xs) _ = case quote xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "cons")):xs) _ = case cons xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "car")):xs) _ = case car xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "cdr")):xs) _ = case cdr xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "length")):xs) _ = case lengths xs of
    Right res -> Right res
    Left error -> Left error
evalList ((ExprAtom (AtomStr "null?")):xs) _ = case nulls xs of
    Right res -> Right res
    Left error -> Left error
evalList _ _ = Left "command not found"

eval :: [SExpr] -> Env -> Either String [SExpr]
eval ((ExprList x):xs) env = case evalList x env of
    Right res -> Right res
    Left error -> Left error
eval _ _ = Left "error"

