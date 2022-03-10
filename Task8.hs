
import Type ( Goal, Prog(..) ) 
import Task2 ( Pretty(pretty) ) 
import Task4 ( Subst ) 
import Task7 ( Strategy, sld, dfs, bfs, solveWith ) 
import Parser ( parseFile, parse ) 
import Data.Either () 



interaktiveUmgebung :: IO()
interaktiveUmgebung =   do 
                        putStrLn "Willkommen!"
                        interactiveEnviroment (Prog []) dfs ""

helpMsg :: String 
helpMsg = "Commands available from the prompt:\n" ++
           "<goal>      Solves/proves the specified goal.\n" ++
           ":h          Shows this help message.\n" ++
           ":l <file>   Loads the specified file.\n" ++
           ":p          Prints the currently loaded program.\n" ++
           ":q          Exits the interactive environment.\n" ++
           ":r          Reloads the last loaded file.\n" ++
           ":s <strat>  Sets the specified search strategy\n" ++
           "            where <strat> is one of 'dfs', 'bfs', or 'iddfs'.\n" ++
           ":t <goal>   Prints the SLD tree for the specified goal."

interactiveEnviroment :: Prog -> Strategy -> String -> IO()
interactiveEnviroment prog strat filePath = do
     putStr "?- "
     l <- getLine
     if length (words l) > 0 
     then case (words l) !! 0 of
         ":h" ->  do
                  putStrLn (helpMsg) 
                  interactiveEnviroment prog strat filePath
         ":l" ->  do
                  parsed <- parseFile (tail (tail (tail l)))
                  case parsed of
                     Right a  -> do
                                 putStrLn "File loaded!"
                                 interactiveEnviroment a strat (tail (tail (tail l)))
                     Left a   -> do 
                                 putStrLn a
                                 interactiveEnviroment prog strat filePath
         ":p" ->  case prog of
                     (Prog []) ->   do 
                                    putStrLn "No program loaded!"
                                    interactiveEnviroment prog strat filePath
                     (Prog _) ->    do
                                    putStrLn (pretty prog)
                                    interactiveEnviroment prog strat filePath
         ":q" ->  putStrLn ("Exit!")
         ":r" ->  do
                  y <- (parseFile filePath)
                  case y of
                     Left a  ->     do
                                    putStrLn a
                                    interactiveEnviroment prog strat filePath
                     Right a ->     do
                                    putStrLn "Loaded oder so!"
                                    interactiveEnviroment a strat filePath  
         ":s" ->  case tail (tail (tail l)) of
                     "bfs"       -> do
                                    putStrLn "Strat changed to bfs!"
                                    interactiveEnviroment prog bfs filePath
                     "dfs"       -> do
                                    putStrLn "Strat changed to dfs!"
                                    interactiveEnviroment prog dfs filePath
                     "iddfs"     -> do
                                    putStrLn "not implemented yet!"
                                    interactiveEnviroment prog strat filePath
                     _   -> do
                                    putStrLn "No viable strategy type \":h\" for help!"
                                    interactiveEnviroment prog strat filePath
         ":t" ->  case parse (tail (tail (tail l))) :: Either String Goal of
                     Left a ->   do
                                 putStrLn a
                                 interactiveEnviroment prog strat filePath
                     Right a ->  do
                                 putStrLn ( pretty (sld prog a))
                                 interactiveEnviroment prog strat filePath
         _    ->  case parse l :: Either String Goal of 
                     Left a ->   do
                                 putStrLn a
                                 interactiveEnviroment prog strat filePath
                     Right a ->  do                                  
                                 case (solveWith prog a strat) of
                                    []          -> putStrLn "No solution."
                                    _   -> solver (solveWith prog a strat)
                                 
                                 interactiveEnviroment prog strat filePath
      else interactiveEnviroment prog strat filePath


solver :: [Subst] -> IO ()
solver [] =     putStrLn "No more solutions."
solver (s:ss) = do
                putStr (pretty s ++ " ")
                c <- getLine
                case c of
                   ";" -> solver ss
                   _   -> putStr ""