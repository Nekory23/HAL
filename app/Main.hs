module Main where

import System.Environment (getArgs)
import Text.Printf (printf)
import System.IO ( stderr, hPutStrLn )
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith, exitFailure)

import ErrorHandling
import Values
import GetContent
import ParseFile
import Parser
import Eval

-------------------------------------------------------------------
--displayContent :: [String] -> IO ()
--displayContent xs = foldr ((>>) . print) (return ()) xs
-------------------------------------------------------------------
--
--
--displayAtom :: Atom -> IO ()
--displayAtom (AtomNum x) = print x
--displayAtom (AtomBool x) = print x
--displayAtom (AtomStr x) = print x
--displayAtom AtomNil = print "()"
--
--displayList :: [SExpr] -> IO ()
--displayList [] = return ()
--displayList ((ExprAtom x):xs) = displayAtom x >> displayList xs
--displayList (_:xs) = displayList xs
--
--displayResult :: [SExpr] -> IO ()
--displayResult [] = return ()
--displayResult [ExprAtom _] = return ()
--displayResult ((ExprAtom _):_:_) = return ()
--displayResult ((ExprList x):xs) = displayList x >> displayResult xs

displayError :: String -> IO a
displayError error =
    hPutStrLn stderr ("Error: " ++ error) >> exitWith (ExitFailure 84)

displayErrorRepl :: String -> IO ()
displayErrorRepl error = hPutStrLn stderr ("Error: " ++ error)

displayList :: [SExpr] -> IO ()
displayList [] = putStrLn ")"
displayList (ExprList x:xs) = displayList x
displayList (ExprAtom (AtomBool False):xs) = 
    if null xs 
    then putStr "#f" >> displayList xs
    else putStr "#f " >> displayList xs
displayList (ExprAtom (AtomBool True):xs) =
    if null xs 
    then putStr "#t" >> displayList xs
    else putStr "#t " >> displayList xs
displayList (ExprAtom AtomNil:xs) = 
    if null xs 
    then putStr "()" >> displayList xs
    else putStr "() " >> displayList xs
displayList (ExprAtom (AtomStr x):xs) = 
    if null xs 
    then putStr x >> displayList xs
    else putStr (x ++ " ") >> displayList xs
displayList (ExprAtom (AtomNum x):xs) =
    if null xs
    then putStr (show x) >> displayList xs
    else putStr (show x ++ " ") >> displayList xs

displayRes :: [SExpr] -> IO ()
displayRes [] = return ()
displayRes ((ExprList x):xs) = putStr "(" >> displayList x >> putStr ")"
displayRes (ExprAtom (AtomNum x):xs) = print x >> displayRes xs
displayRes (ExprAtom (AtomBool True):xs) = putStrLn "#t" >> displayRes xs
displayRes (ExprAtom (AtomBool False):xs) = putStrLn "#f" >> displayRes xs
displayRes (_:xs) = displayRes xs

launchRepl :: Env -> IO ()
launchRepl env = do
    content <- getLine
    case parseLine content of
        Right parsed -> case eval [parsed] env of
            Right res -> displayRes res >> launchRepl env
            Left error -> displayErrorRepl error >> launchRepl env
        Left error -> displayErrorRepl error >>  launchRepl env

gestArgs :: Bool -> [String] -> Env -> IO ()
gestArgs True [] env = launchRepl env
gestArgs False files env =
    let content = getFilesContent files []
    in case parseFiles content [] of
        Right parsed -> case eval parsed env of
            Right res -> displayRes res
            Left error -> displayError error
        Left error -> displayError error
gestArgs True files env = return () -- stock the files in env and launch rep

main :: IO ()
main = do
    args <- getArgs
    let arg = Arguments [] False
        env = Env [] []
    case checkArgs args arg of
        Right arg' -> gestArgs (i arg') (files arg') env
        Left error -> displayError error