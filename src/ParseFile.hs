module ParseFile where

import Parser

displayLine :: [Atom] -> IO ()
displayLine [] = return ()
displayLine [AtomBool _] = return ()
displayLine ((AtomBool _):_:_) = return ()
displayLine [AtomNum _] = return ()
displayLine ((AtomNum _):_:_) = return ()
displayLine [AtomNil] = return ()
displayLine (AtomNil:_:_) = return ()
displayLine (AtomStr x:xs) = print x >> displayLine xs

displayParsedContent :: [[Atom]] -> IO ()
displayParsedContent = foldr ((>>) . displayLine) (return ())
---------------------------------

parseLine :: String -> Either String SExpr
parseLine str = case runParser parseListExpr str of
    Just (res, str') -> if null str'
        then Right res else Left "error parsing"
    Nothing ->  Left "error parsing"

parseFiles :: [String] -> [SExpr] -> Either String [SExpr]
parseFiles [] [] = Right []
parseFiles [] list = Right list
parseFiles (x:xs) list = case parseLine x of
    Right parsed -> parseFiles xs (list ++ [parsed])
    Left error -> Left error