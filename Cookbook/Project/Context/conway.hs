import System.IO
import System.Environment
import System.Process
import Cookbook.Project.Context.Context
import Cookbook.Essential.IO

main =
  do
    (dir:fileName:_) <- getArgs
    allLines <- filelines fileName
    commands <- skim dir
    mapM_ system (parseContexts commands allLines)
    
