import Data.Char
import FPPrac.Trees
import Data.List
import Debug.Trace
import System.IO
import System.Environment
import Parse
import Grammar
import Checker

start :: FilePath -> IO ()
start input = do
	inHandle <- openFile input ReadMode  
	contents <- hGetContents inHandle
	putStrLn ("SDSS: " ++ contents)
	outHandle <- openFile "out.hs" WriteMode
	hPutStr outHandle $ toSprockell [] $ convert $ parse grammar Program $ tokens contents
	hClose inHandle
	hClose outHandle

toSprockell :: [(String, Int)] -> Tree -> String
toSprockell list tree = 
	case tree of
		(DoubloonNode t1 t2)-> 	spacing ++ "-- doubloon " ++ (getValue t1) ++ " be " ++ 
								printNode list t2 ++ " RegA,\n" ++ 
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"
		
		(BoolNode t1 t2)	-> 	spacing ++ "-- bool " ++ (getValue t1) ++ " be " ++ 
								printNode list t2 ++ " RegA,\n" ++ 
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"

		(OpNode s t1 t2)	->	(getValue t1) ++ " " ++ s ++ " " ++ (getValue t2) ++ "\n" ++
								spacing ++ pNode list t1 ++ " RegA,\n" ++
								spacing ++ pNode list t2 ++ " RegB,\n" ++
								spacing ++ "Compute " ++ (getOp s) ++ " RegA RegB RegA,\n" ++
								spacing ++ "Push RegA,\n"

		(IfNode (BoolExNode (Comp "be" t1 t2)) xs)	-> 	spacing ++ "-- parley(" ++ (getValue t1) ++ " be " ++ getValue(t2) ++ ")\n" ++ 
														spacing ++ pNode list t2 ++ " RegA,\n" ++
														spacing ++ pNode list t1 ++ " RegB,\n" ++
														spacing ++ "Compute NEq RegB RegA RegA\n" ++
														spacing ++ "Branch RegA Rel(" ++ (show (calcLen xs)) ++ ")\n" ++
														(concat (map (toSprockell list) xs))

		(ZupaNode s xs)			-> 	"import Sprockell.System\n\nprog = [\n" ++ 
									(concat (map (toSprockell (addToList xs 0)) xs)) ++ 
									spacing ++ "-- END\n" ++ 
									spacing ++ "EndProg\n" ++ 
									spacing ++ "]\n\nmain = run 1 prog"
		_						-> 	""

printNode :: [(String, Int)] -> Tree -> String
printNode list t@(VarNode _ _)	= (getValue t) ++ "\n" ++ spacing ++ load list t
printNode list t 				= toSprockell list t ++ spacing ++ "Pop "

pNode :: [(String, Int)] -> Tree -> String
pNode list t@(VarNode _ _)	= load list t
pNode list t 				= toSprockell list t ++ spacing ++ "Pop "

load :: [(String, Int)] -> Tree -> String
load list t | getInt list t /= 0 	= "Load (Addr " ++ (show (getInt list t)) ++ ")"
			| otherwise				= "Const " ++ (getValue t)

getOp :: String -> String
getOp "+" = "Add"
getOp "-" = "Sub"
getOp "*" = "Mul"
getOp "/" = "Div"

calcLen :: [Tree] -> Int
calcLen ((DoubloonNode (VarNode _ _) (VarNode _ _)):xs)	= 3 + calcLen xs
calcLen ((DoubloonNode (VarNode _ _) xs):xs')			= 3 + calcLen [xs] + calcLen xs'
calcLen ((BoolNode (VarNode _ _) (VarNode _ _)):xs) 	= 3 + calcLen xs
calcLen ((OpNode _ (VarNode _ _) (VarNode _ _)):xs) 	= 5 + calcLen xs 
calcLen ((IfNode _ xs):xs')								= 5 + calcLen xs + calcLen xs'
calcLen _												= 0

addToList :: [Tree] -> Int -> [(String, Int)]
addToList [] _										= []
addToList ((BootyNode (VarNode s l) t): xs)	i		= (s, (i+1)): addToList xs (i+1)
addToList ((DoubloonNode (VarNode s l) t): xs) i 	= (s, (i+1)): addToList xs (i+1)
addToList ((BoolNode (VarNode s l) t): xs) i		= (s, (i+1)): addToList xs (i+1)
addToList ((TreasureNode (VarNode s l) t): xs) i 	= (s, (i+1)): addToList xs (i+1)
addToList ((IfNode t1 xs): xs') i 					= list ++ addToList xs' (snd (last list) + 1)
													where
														list = addToList xs i
addToList ((ElseNode xs): xs') i 					= list ++ addToList xs' (snd (last list) + 1)
													where
														list = addToList xs i
addToList ((WhileNode t1 xs): xs') i 				= list ++ addToList xs' (snd (last list) + 1)
													where
														list = addToList xs i
addToList ((ForNode t1 _ _ xs): xs') i 				= list' ++ list ++ addToList xs' (snd (last list) + 1)
													where
														list'= addToList [t1] i
														list = addToList xs (snd (last list') + 1)
addToList ((FuncNode s xs xs'): xs'') i 			= list ++ list' ++ addToList xs'' (snd (last list') + 1)
													where
														list = addToList xs i
														list'= addToList xs' (snd (last list) + 1)
addToList _ i 										= []

getInt :: [(String, Int)] -> Tree -> Int
getInt [] _ 						= 0
getInt ((s,i):list) n@(VarNode nS _)| s == nS	= i
									| otherwise = getInt list n
getInt list _ 						= 0

spacing :: String
spacing = "       "