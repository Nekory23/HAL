module GetContent where

import System.IO.Unsafe ( unsafePerformIO )

getFileContent :: String -> [String]
getFileContent file = lines (unsafePerformIO . readFile $ file)

getFilesContent :: [String] -> [String] -> [String]
getFilesContent [] content = content
getFilesContent (x:xs) content = getFilesContent xs (content ++ getFileContent x)