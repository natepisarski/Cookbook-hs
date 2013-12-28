module Cookbook.Project.Quill.Quill(tblNames,tables,getTable,lookUp,createTable,removeTable,removeItem,addItem,tableToString,changeItem) where
import qualified Cookbook.Essential.Common         as Cm
import qualified Cookbook.Essential.Continuous     as Ct
import qualified Cookbook.Ingredients.Lists.Remove as Rm
import qualified Cookbook.Ingredients.Lists.Modify as Md
import qualified Cookbook.Ingredients.Lists.Access as Ac
import qualified Cookbook.Ingredients.Lists.Splice as Sp
import qualified Cookbook.Ingredients.Tupples.Look as Lk
import qualified Cookbook.Recipes.Sanitize         as Sn

--Custom data
data Table = Table {name :: String, info :: [(String,String)]}

-- Parsing
prepare :: [String] -> String
prepare = ((flip Sp.splice) ("(*","*)")) . ((flip Sn.blacklist) [' ','\n']) . Cm.flt

tokenize :: [a] -> [(a,a)]
tokenize []  = []
tokenize [x] = []
tokenize (x:y:xs) = (x,y) : tokenize xs

-- Table access
tblNames :: String -> [String]
tblNames x = (Ct.before x '{') : Md.surroundedBy x ('}','{')


headlessData :: String -> [[String]]
headlessData x = map ((flip Md.splitOn) ';') $  (Md.surroundedBy x ('{','}'))

tokenLists :: [[String]] -> [[(String,String)]]
tokenLists x =  map tokenize $ map Cm.flt ( map (map ((flip Md.splitOn) ':')) x)


-- API functions
tables :: [String] -> [(String,[(String,String)])]
tables x = let y = prepare x in
   zip (tblNames y) $ tokenLists (headlessData y)

getTable :: [(String,[(String,String)])] -> String -> [(String,String)]
getTable x c = Cm.flt $ Lk.lookList x c

lookUp :: [(String,[(String,String)])] -> (String,String) -> [String]
lookUp x (c,d) = ((flip Lk.lookList) d) $ getTable x c

createTable :: [(String,[(String,String)])] -> String -> [(String,[(String,String)])]
createTable x c = (c,[]) : x

removeTable :: [(String,[(String,String)])] -> String -> [(String,[(String,String)])]
removeTable x c = [e | e@(d,_) <- x, d /= c]

removeItem :: [(String,[(String,String)])] -> (String,String) -> [(String,[(String,String)])]
removeItem [] _ = []
removeItem ((tbl,ite):re) (tb,it)
  | tbl == tb = (tbl,filter (\(c,d) -> c /= it) ite) : removeItem re (tb,it)
  | otherwise = (tbl,ite) : removeItem re (tb,it)
                
addItem :: [(String,[(String,String)])] -> String -> (String,String) -> [(String,[(String,String)])]
addItem [] _ _ = []
addItem ((a,b):c) tb it = if a == tb then (a,it:b) : addItem c tb it else (a,b) : addItem c tb it

changeItem :: [(String,[(String,String)])] -> (String,String) -> String -> [(String,[(String,String)])]
changeItem db (tb,it) c = addItem (removeItem db (tb,it)) tb (it,c)

tableToString :: (String,[(String,String)]) -> String
tableToString (x,b) = x ++ "{" ++ (Cm.flt (map detokenize b)) ++ "}"
  where detokenize (a,b) = a ++ ":" ++ b ++ ";"


