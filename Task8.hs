import Type
import Task7
import Task2
import Parser
import Data.Either

interaktiveUmgebung :: IO()
interaktiveUmgebung = interactiveLoop (Prog []) dfs ""


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
     putStrLn "?- "
     l <- getLine  
     if (  l !! 0 == ':') then 
      case l !! 1 of
         'h'  -> do putStrLn (helpMsg) 
                    interactiveLoop (Prog rules) strat filePath
         'p'  -> if rules == [] 
                 then do 
                    putStrLn "No program loaded." 
                    interactiveLoop (Prog rules) strat filePath
                 else do
                    putStrLn (pretty (Prog rules)) 
                    interactiveLoop (Prog rules) strat filePath
         'q'  -> putStrLn ("exit.") 
         'r'  -> if filePath == "" 
                 then do 
                    putStrLn "No file loaded." 
                    interactiveLoop (Prog rules) strat filePath
                 else if isRight (parse (tail (tail (tail l)))) 
                    then do  
                        putStrLn "Loaded."
                        interactiveLoop (fromRight (Prog []) (parseFile filePath)) strat filePath
                    else do
                        putStrLn (fromLeft "" (parse (tail (tail (tail l))))) 
                        interactiveLoop (Prog rules) strat filePath                       
         'l'  -> if isLeft (parseFile (tail (tail (tail l)))) then putStrLn ("Could not read file.") else 
                 do 
                    putStrLn "Loaded."
                    interactiveLoop (parseFile (tail (tail (tail l))) ) strat (tail (tail (tail l)))
         's'  -> interactiveLoop (Prog rules) (parse rest) filePath

         't'  -> if isRight (parse (tail (tail (tail l))))
                  then do 
                     putStrLn ("Could not read Strategy.")
                     interactiveLoop (Prog rules) strat filePath
                  else do
                     putStrLn ( pretty (sld (Prog rules) (fromRight (Goal []) (parse (tail (tail (tail l))))) ) ) 
                     interactiveLoop (Prog rules) strat filePath
      else putStrLn("")

getRigth :: IO (Either String a)
getRigth (Right a) = a