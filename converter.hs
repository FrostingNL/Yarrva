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
		(DoubloonNode t1 t2)	-> 	spacing ++ "-- doubloon " ++ (getValue t1) ++ " be " ++ getValue(t2) ++ "\n" ++ 
									spacing ++ "Const " ++ (getValue t2) ++ " RegA,\n" ++ 
									spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"
		(BoolNode t1 t2)		-> 	spacing ++ "-- bool " ++ (getValue t1) ++ " be " ++ getValue(t2) ++ "\n" ++ 
									spacing ++ "Const " ++ (getBool t2) ++ " RegA,\n" ++ 
									spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"

		(IfNode (BoolExNode (Comp "be" t1 t2)) xs)		-> 	spacing ++ "-- parley(" ++ (getValue t1) ++ " be " ++ getValue(t2) ++ ")\n" ++ 
															spacing ++ "Const " ++ (getValue t2) ++ " RegA,\n" ++ 
															spacing ++ "Load (Addr " ++ (show (getInt list t1)) ++ ") RegB,\n" ++
															spacing ++ "Compute NEq RegB RegA RegA\n" ++
															spacing ++ "Branch RegA Rel(" ++ (show (calcLen xs)) ++ ")\n" ++
															(concat (map (toSprockell list) xs))

		(ZupaNode s xs)			-> 	"import Sprockell.System\n\nprog = [\n" ++ 
									(concat (map (toSprockell (addToList xs 0)) xs)) ++ 
									spacing ++ "EndProg\n" ++ 
									spacing ++ "]\n\nmain = run 1 prog"
		_						-> 	""

calcLen :: [Tree] -> Int
calcLen ((DoubloonNode _ _):xs)	= 3 + calcLen xs
calcLen ((BoolNode _ _):xs) 	= 3 + calcLen xs
calcLen ((IfNode _ xs):xs')		= 5 + calcLen xs'
calcLen _						= 0

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

getBool :: Tree -> String
getBool (VarNode s l) | s == "Aye" = "1"
					  | otherwise  = "0"

spacing :: String
spacing = "       "