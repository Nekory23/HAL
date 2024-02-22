module Bootstrap where

import System.IO ( stderr, hPutStrLn )

import Parser

-- do again but better (handle uppercase and mix + numbers)
parseElem :: Parser String
parseElem = Parser f where
    f str = case runParser (parseMany (parseAnyChar ['a'..'z'])) str of
        Just ([], rest) -> Nothing
        Just (word, rest) -> Just (word, rest)

parseWord :: String -> Parser String
parseWord str = Parser (f str) where
    f [] list = Just ("ok", list)
    f (x:xs) list = case runParser (parseChar x) list of
        Just (parsed, rest) -> case f xs rest of
            Just (parsed', rest') -> Just (parsed:parsed', rest')
            Nothing -> Nothing
        Nothing -> Nothing

parseQuote :: Parser String
parseQuote = Parser f where
    f [] = Nothing
    f ('\'':xs) = Just ("quote", xs)
    f str = case runParser (parseWord "quote") str of
        Just (word, rest) -> Just ("quote", rest)
        Nothing -> Nothing

-- =========================================== --

-- exo 1

displayElems :: [String] -> IO ()
displayElems [] = return ()
displayElems [x] = putStr x >> displayElems []
displayElems (x:xs) = putStr (x ++ " ") >> displayElems xs

displayPairs :: [String] -> IO ()
displayPairs list =  putStr "(" >> displayElems list >> putStrLn ")"

getElemDotted :: String -> [String] -> [String]
getElemDotted str list = ["foo", "bar"]

fromPairs :: String -> IO ()
fromPairs str = case runParser parseQuote str of
    Just (_, str') -> case runParser (parseChar '(') str' of
        Just (_, str'') -> displayPairs $ getElemDotted str'' []
        Nothing -> hPutStrLn stderr "error ("
    Nothing -> hPutStrLn stderr "error quote"


-- exo 2

displayList :: [String] -> Int -> IO ()
displayList [] 0 = putStrLn ""
displayList [] length = putStr ")" >> displayList [] (length - 1)
displayList [x] l = putStr ("(" ++ x ++ " . ()") >> displayList [] l
displayList (x:xs) l = putStr ("(" ++ x ++ " . ") >> displayList xs l

getElems :: String -> [String] -> Either String [String]
getElems str list = case runParser parseElem str of
    Just (elem, str') -> case runParser (parseMany (parseAnyChar " \t")) str' of
        Just (parsed, rest) -> getElems rest (list ++ [elem])
    Nothing -> case runParser (parseChar ')') str of
        Just (_, str') -> Right list
        Nothing -> Left "error )"

toPairs :: String -> IO ()
toPairs str = case runParser parseQuote str of
    Just (_, str') -> case runParser (parseChar '(') str' of
        Just (_, str'') -> let list = getElems str'' [] in
            case list of 
                Right list' -> displayList list' (length list')
                Left error -> hPutStrLn stderr error
        Nothing -> hPutStrLn stderr "error ("
    Nothing -> hPutStrLn stderr "error quote"

-------------------------------------------------

--test :: [Atom]
--test = [AtomBool True, AtomNil, AtomNum 67]
--
--findNbr :: [Atom] -> Int
--findNbr [] = 0
--findNbr (AtomNum x:xs) = x
--findNbr (_:xs) = findNbr xs