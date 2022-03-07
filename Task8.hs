import Type ( Goal(Goal), Prog(..) )
import Task7 ( Strategy, sld, dfs, bfs, solveWith )
import Task2 ( Pretty(pretty) )
import Parser ( parse, parseFile )
import Data.Either ( isLeft, fromLeft, fromRight, isRight )
import Task4 (Subst(Subst))

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

interactiveLoop :: Prog -> Strategy -> String -> IO()
interactiveLoop (Prog rules) strat filePath = do
     putStr "?- "
     l <- getLine
     if (  l !! 0 == ':') then
      case l !! 1 of
         'h'  -> do putStrLn (helpMsg) 
                    interactiveLoop (Prog rules) strat filePath
         'p'  -> if isEmpti rules
                 then do 
                    putStrLn "No program loaded." 
                    interactiveLoop (Prog rules) strat filePath
                 else do
                    putStrLn (pretty (Prog rules)) 
                    interactiveLoop (Prog rules) strat filePath
         'q'  -> putStrLn ("exit.") 
         'r'  -> if filePath == "" 
                 then do 
                    putStrLn "No file loaded yet." 
                    interactiveLoop (Prog rules) strat filePath
                 else if isRight (parse (tail (tail (tail l))) :: Either String Prog )
                    then do  
                        putStrLn "Loaded."
                        parse <- parseFile filePath 
                        if (isRight  parse) then 
                          do
                              putStrLn "Reloaded." 
                              interactiveLoop (fromRight (Prog []) (parse)) strat filePath
                        else putStrLn "Loading failed!"

                    else do
                        putStrLn (fromLeft "" (parse (tail (tail (tail l))) :: Either String Prog))
                        interactiveLoop (Prog rules) strat filePath                       
         'l'  -> do
                  parse <- parseFile (tail (tail (tail l)))
                  if isRight parse
                  then 
                     do
                     putStrLn "File loaded!"
                     interactiveLoop (fromRight (Prog []) (parse)) strat (tail (tail (tail l)))
                  else 
                     do
                     putStrLn "Could not read File!"
                     interactiveLoop (Prog rules) strat filePath
         's'  -> if (tail (tail (tail l))) == "dfs" 
                    then do 
                       putStrLn "Strategy set to depth-first search."
                       interactiveLoop (Prog rules) dfs filePath
                    else if (tail (tail (tail l))) == "bfs"
                       then do 
                          putStrLn "Strategy set to breadth-first search."
                          interactiveLoop (Prog rules) bfs filePath
                       else if (tail (tail (tail l))) == "iddfs"
                          then do 
                             putStrLn "Strategy set to breadth-first search."
                             interactiveLoop (Prog rules) dfs filePath -- was ist iddfs ?????????????????? TODO !
                          else do
                             putStrLn "Type \":h\" for help."
                             interactiveLoop (Prog rules) strat filePath

         't'  -> if isRight (parse (tail (tail (tail l))) :: Either String Prog)
                  then do 
                     putStrLn ("Could not read Strategy.")
                     interactiveLoop (Prog rules) strat filePath
                  else do
                     putStrLn ( pretty (sld (Prog rules) (fromRight (Goal []) (parse (tail (tail (tail l))))) ) ) 
                     interactiveLoop (Prog rules) strat filePath
      else putStrLn("")

isEmpti :: [a] -> Bool 
isEmpti [] = True 
isEmpti _ = False

interactiveEnviroment :: Prog -> Strategy -> String -> IO()
interactiveEnviroment prog strat filePath = do
     putStr "?- "
     l <- getLine
     case (words l) !! 0 of
         ":h" ->  do
                  putStrLn (helpMsg) 
                  interactiveEnviroment prog strat filePath
         ":l" ->  do
                  parse <- parseFile (tail (tail (tail l)))
                  case parse of
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
                     otherwise   -> do
                                    putStrLn "No viable strategy type \":h\" for help!"
                                    interactiveEnviroment prog strat filePath
         ":t" ->  case parse (tail (tail (tail l))) :: Either String Goal of
                     Left a ->   do
                                 putStrLn a
                                 interactiveEnviroment prog strat filePath
                     Right a ->  do
                                 putStrLn ( pretty (sld prog a))
                                 interactiveEnviroment prog strat filePath
         otherwise -> case parse l :: Either String Goal of 
                     Left a ->   do
                                 putStrLn a
                                 interactiveEnviroment prog strat filePath
                     Right a ->  do
                                 solver (solveWith prog a strat)
                                 interactiveEnviroment prog strat filePath

solver :: [Subst] -> IO ()
solver [] =     putStrLn "No more solutions."
solver (s:ss) = do
                putStr (pretty s ++ " ")
                c <- getChar
                case c of
                   ';' -> solver ss
                   otherwise -> putStr ""

