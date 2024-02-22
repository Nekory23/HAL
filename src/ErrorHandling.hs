module ErrorHandling where

import Values

checkValid :: String -> String -> Arguments -> Either String Arguments
checkValid _ [] _ = Left "argument is not a LISP file"
checkValid file ['.', 's', 'c', 'm'] arg = 
    let fileList = files arg ++ [file] in
    Right (arg{files = fileList})
checkValid file (x:xs) arg = checkValid file xs arg

checkFiles :: [String] -> Arguments -> Either String Arguments
checkFiles [] arg = Right arg
checkFiles ["-i"] arg = Right (arg{i = True})
checkFiles (x:xs) arg = case checkValid x x arg of
    Right arg -> checkFiles xs arg
    Left error -> Left error

checkArgs :: [String] -> Arguments -> Either String Arguments
checkArgs [] arg = Right (arg{i = True})
checkArgs ["-i"] _ = Left "-i must be used with files"
checkArgs args arg = case checkFiles args arg of
    Right arg -> Right arg
    Left error -> Left error