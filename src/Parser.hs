module Parser where

import Data.Maybe
import Control.Monad (Functor)
import Control.Applicative

data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

data Env = Env {
    envVariables :: [(String, Atom)],
    envFunctions :: [(String, [Atom])]
    --result :: [Atom] -- ?
}

data SExpr = ExprAtom Atom | ExprList [SExpr] deriving (Show)
data Atom = AtomBool Bool | AtomNum Int | AtomStr String | AtomNil deriving (Show)

instance Functor Parser where
    fmap fct parser = Parser f where
        f [] = Nothing
        f str = case runParser parser str of
            Just (res, str') -> Just (fct res, str')
            Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser (\str -> Just(x, str))
    p1 <*> p2 = Parser f where
        f str = case runParser p1 str of
            Just (p, rest) -> case runParser p2 rest of
                Just (p', rest') -> Just (p p', rest')
                Nothing -> Nothing
            Nothing -> Nothing

instance Alternative Parser where
    empty = Parser (const Nothing)
    p1 <|> p2 = Parser f where
        f str = case runParser p1 str of
            Just res -> Just res
            Nothing -> runParser p2 str <|> Nothing

-----------------------
-- PARSER EVAL EXPER --
-----------------------

-- parse a specific char
parseChar :: Char -> Parser Char
parseChar char = Parser f where
    f [] = Nothing
    f (x:xs)
        | x == char = Just (x, xs)
        | otherwise = Nothing

-- parse any char from the given str
-- use elem
parseAnyChar :: String -> Parser Char
parseAnyChar str = Parser (f str) where
    f [] [] = Nothing
    f [] _ = Nothing
    f _ [] = Nothing
    f (x:xs) list@(y:ys)
        | isNothing $ runParser (parseChar x) list = f xs list
        | otherwise = Just (x, ys)

-- applies both the first or second parse
parseOr :: Parser a -> Parser a -> Parser a
parseOr p1 p2 = Parser f where
    f str = case runParser p1 str of
        Nothing -> runParser p2 str
        Just res -> Just res

-- applies both the first and second parse
parseAnd :: Parser a -> Parser b -> Parser (a,b)
parseAnd p1 p2 = Parser f where
    f str = case runParser p1 str of
        Just (parsed, rest) -> case runParser p2 rest of
            Just (parsed', rest') -> Just ((parsed, parsed'), rest')
            Nothing -> Nothing
        Nothing -> Nothing

-- applies both the first and second parse and applies a function to the result
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith func p1 p2 = Parser f where
    f str = case runParser p1 str of
        Just (parsed, rest) -> case runParser p2 rest of
            Just (parsed', rest') -> Just (func parsed parsed', rest')
            Nothing -> Nothing
        Nothing -> Nothing

-- tries to apply it until it fails (parseMany itself can never fail)
parseMany :: Parser a -> Parser [a]
parseMany p = Parser (f p) where
    f p str = case runParser p str of
        Just (parsed, rest) -> case f p rest of
            Just (parsed', rest') -> Just (parsed:parsed', rest')
            Nothing -> Just ([parsed], rest)
        Nothing -> Just ([], str)

-- parseMany but must parse at least 1 elem
parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> (parseSome p <|> pure [])

-- parse an unsigned int
parseUInt :: Parser Int
parseUInt = read <$> parseSome (parseAnyChar ['0'..'9'])

-- parse a signed int
parseInt :: Parser Int
parseInt = Parser f where
    f ('-':rest) = case runParser parseUInt rest of
        Just (nbr, rest') -> Just (nbr * (-1), rest')
        Nothing -> Nothing
    f str = runParser parseUInt str

-- parse an unsigned float
parseUFloat :: Parser Float
parseUFloat = Parser f where
    f str = case runParser (parseSome (parseAnyChar ['0'..'9'])) str of
        Just (parsed, rest) -> case runParser (parseChar '.') rest of
            Just (p, rest') ->
                case runParser (parseSome (parseAnyChar ['0'..'9'])) rest' of
                    Just (parsed', rest'') ->
                        let nbr = parsed ++ [p] ++ parsed' in
                        Just (read nbr::Float, rest'')
                    Nothing -> Nothing
            Nothing -> Just (read parsed::Float, rest)
        Nothing -> Nothing

-- parse a float
parseFloat :: Parser Float
parseFloat = Parser f where
    f ('-':rest) = case runParser parseUFloat rest of
        Just (nbr, rest') -> Just (nbr * (-1), rest')
        Nothing -> Nothing
    f str = runParser parseUFloat str

-- parse a tuple
parseTuple :: Parser a -> Parser (a,a)
parseTuple p = Parser f where
    f str = case runParser (parseChar '(') str of
        Just (_, str') -> case runParser p str' of
            Just (nbr, str'') -> case runParser (parseChar ',') str'' of
                Just (_, str''') -> case runParser p str''' of
                    Just (nbr2, rest) -> case runParser (parseChar ')') rest of
                        Just (_, rest') -> Just ((nbr, nbr2), rest')
                        Nothing -> Nothing
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing
        Nothing -> Nothing

----------------
-- PARSER HAL --
----------------

parseExpr :: Parser SExpr
parseExpr = ExprAtom <$> parseAtom <|> parseListExpr

parseElem :: Parser SExpr
parseElem = parseExpr <* parseSpaces

parseListExpr :: Parser SExpr
parseListExpr =
    ExprList <$> (parseChar '(' *> parseSome parseElem <* parseChar ')')

-- parser Atoms --
parseAtom :: Parser Atom
parseAtom = parseSpaces *> parseAtomBool
        <|> parseSpaces *> parseAtomNil
        <|> parseSpaces *> parseAtomNbr
        <|> parseSpaces *> parseAtomStr

parseAtomBool :: Parser Atom
parseAtomBool = AtomBool <$> parseBool

parseAtomNil :: Parser Atom
parseAtomNil = AtomNil <$ parseNil

parseAtomNbr :: Parser Atom
parseAtomNbr = AtomNum <$> parseInt

parseAtomStr :: Parser Atom
parseAtomStr = AtomStr <$> parseQuote <|> AtomStr <$> parseStr

-- parser Types --
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
        Just (_, rest) -> Just ("quote", rest)
        Nothing -> Nothing

parseStr :: Parser String
parseStr = parseSome (parseAnyChar ['a'..'z'] <|> parseAnyChar "\"?")
    <|> parseSome (parseAnyChar "+-*<>=")

parseBool :: Parser Bool
parseBool = (== 't') <$> (parseChar '#' *> parseAnyChar "tf")

parseNil :: Parser ()
parseNil = () <$ (parseChar '(' *> parseChar ')')

-- parser spaces --
parseSpaces :: Parser String
parseSpaces = Parser f where
    f str = case runParser (parseMany (parseAnyChar " \t\n")) str of
        Just (res, rest) -> Just (res, rest)
        Nothing -> Nothing