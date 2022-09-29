-- Started: March 19, 2018
import Data.List
import System.Directory

main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  name <- getLine
  if null name then main' else putStrLn $ "Hi, " ++ name ++ "!"


removeNotify :: String -> IO ()
removeNotify s
  | null s = return ()
  | otherwise = do
      putStrLn $ "Removing file: " ++ s
      removeFile s

main'' :: IO ()
main'' = do
  putStr "Substring: "
  s <- getLine
  case s of
    "" -> putStrLn "Canceled"
    _ -> do
      fs <- getDirectoryContents "."
      let fs' = filter (isInfixOf s) fs
      mapM_ removeNotify fs'
