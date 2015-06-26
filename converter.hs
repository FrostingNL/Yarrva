import Data.Char
import FPPrac.Trees
import Data.List
import Debug.Trace
import System.IO
import System.Environment
import Parse
import Grammar
import Checker

main = start "Example programs/test.yarr"

start :: FilePath -> IO ()
start input = do
	inHandle <- openFile input ReadMode  
	contents <- hGetContents inHandle
	putStrLn (contents)
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

		(AssignNode t1 t2) 	-> 	spacing ++ "-- " ++ (getValue t1) ++ " be " ++ 
								printNode list t2 ++ " RegA,\n" ++
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"
 
		(OpNode s t1 t2)	->	(getValue t1) ++ " " ++ s ++ " " ++ (getValue t2) ++ "\n" ++
								spacing ++ pNode list t1 ++ " RegA,\n" ++
								spacing ++ pNode list t2 ++ " RegB,\n" ++
								spacing ++ "Compute " ++ (getOp s True) ++ " RegA RegB RegA,\n" ++
								spacing ++ "Push RegA,\n"

		(PrintNode t1) 		->	spacing ++ "-- parrot (" ++ (getValue t1) ++ ")\n" ++
								spacing ++ pNode list t1 ++ " RegA,\n" ++
								spacing ++ "Const (ord '0') RegB,\n" ++ 
								spacing ++ "Compute Add RegA RegB RegB,\n" ++
       							spacing ++ "Write RegB stdio,\n" ++
       							spacing ++ "Read (Addr 0x0),\n" ++
       							spacing ++ "Receive RegB,\n"

		(ZupaNode s xs)		-> 	"import Sprockell.System\n\nprog = [\n" ++ 
								(concat (map (toSprockell (addToList xs 0)) xs)) ++ 
								spacing ++ "-- END\n" ++ 
								spacing ++ "EndProg\n" ++ 
								spacing ++ "]\n\nmain = run 1 prog >> putChar '\\n'"

		(GiftNode t1)		-> 	spacing ++ "-- gift " ++ (getValue t1) ++ "\n" ++
								spacing ++ pNode list t1 ++ " RegA,\n" ++
								spacing ++ "Const 1 RegB,\n" ++
								spacing ++ "Compute Add RegA RegB RegA,\n" ++ 
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"

		(IfNode (BoolExNode t1) xs)			-> 	spacing ++ "-- parley(" ++ boolText t1 ++ ")\n" ++ 
												boolEx t1 list ++
												spacing ++ "Branch RegA (Rel(" ++ (show ((calcLen xs)+1)) ++ ")),\n" ++
												(concat (map (toSprockell list) xs)) 

		(IfElseNode (BoolExNode t1) xs xs')	-> 	spacing ++ "-- parley(" ++ boolText t1 ++ ")\n" ++ 
												boolEx t1 list ++
												spacing ++ "Branch RegA (Rel(" ++ (show ((calcLen xs)+3)) ++ ")),\n" ++
												(concat (map (toSprockell list) xs)) ++ 
												spacing ++ "Jump (Rel(" ++ (show ((calcLen [xs'])+1)) ++ ")),\n" ++ 
												toSprockell list xs'

		(ElseNode xs)						->	spacing ++ "Jump (Rel(" ++ (show ((calcLen xs)+1)) ++ ")),\n" ++ 
												spacing ++ "-- heave\n" ++ 
												(concat (map (toSprockell list) xs))

		(WhileNode (BoolExNode t1) xs)		->	spacing ++ "-- whirlpool(" ++ boolText t1 ++ ")\n" ++ 
												spacing ++ "Compute Add PC Zero RegE,\n" ++
												spacing ++ "Push RegE,\n" ++ 
												boolEx t1 list ++
												spacing ++ "Branch RegA (Rel(" ++ (show ((calcLen xs)+3)) ++ ")),\n" ++
												(concat (map (toSprockell list) xs)) ++
												spacing ++ "Pop RegE,\n" ++
												spacing ++ "Jump (Ind RegE),\n" ++
												spacing ++ "Pop RegE,\n" 

		(ForNode t0 (BoolExNode t1) t3 xs)	-> 	spacing ++ "-- navigate("++ (getValue t0) ++ ". " ++ boolText t1 ++ ". " ++ getValue(t3) ++ ")\n" ++
												toSprockell list t0 ++
												spacing ++ "Compute Add PC Zero RegE,\n" ++
												spacing ++ "Push RegE,\n" ++ 
												boolEx t1 list ++
												spacing ++ "Branch RegA (Rel(" ++ (show ((calcLen xs)+(calcLen [t3])+3)) ++ ")),\n" ++
												(concat (map (toSprockell list) xs)) ++ 
												toSprockell list t3 ++
												spacing ++ "Pop RegE,\n" ++
												spacing ++ "Jump (Ind RegE),\n" ++
												spacing ++ "Pop RegE,\n" 

		n@(FuncNode s xs xs')					->	spacing ++ "-- " ++ s ++ "(" ++ (funcText xs) ++ ")\n" ++
												spacing ++ "Const 3 RegA,\n" ++
												spacing ++ "Compute Add PC RegA RegE,\n" ++ 
												spacing ++ "Store RegE (Addr " ++ (show (getInt list n)) ++ "),\n" ++ 
												spacing ++ "Jump (Rel(" ++ (show ((calcLen xs')+(calcLen xs)+4)) ++ ")),\n" ++
												popFunc list xs ++ 
												(concat (map (toSprockell list) xs')) ++
												spacing ++ "Pop RegE,\n" ++
												spacing ++ "Push RegA,\n" ++	
												spacing ++ "Jump (Ind RegE),\n"

		n@(DoFuncNode s xs)					-> 	spacing ++ "-- " ++ s ++ "(" ++ (funcText xs) ++ ")\n" ++
												spacing ++ "Const " ++ (show ((calcLen xs)+4)) ++ " RegA,\n" ++ 
												spacing ++ "Compute Add PC RegA RegE,\n" ++
												spacing ++ "Push RegE,\n" ++
												pushFunc list xs ++ 
												spacing ++ "Load (Addr " ++ (show (getInt list n)) ++ ") RegA,\n" ++ 
												spacing ++ "Jump (Ind RegA),\n" ++
												spacing ++ "Pop RegA,\n" 
		_					-> 	""

funcText :: [Tree] -> String
funcText [x]    = (getValue x)
funcText (x:xs) = (getValue x) ++ ", " ++funcText xs

pushFunc :: [(String, Int)] -> [Tree] -> String
pushFunc list []  	 =  "" 
pushFunc list (x:xs) = 	spacing ++ pNode list x ++ " RegA,\n" ++ 
						spacing ++ "Push RegA,\n" ++
						pushFunc list xs

popFunc :: [(String, Int)] -> [Tree] -> String
popFunc list []  	= "" 
popFunc list (n:xs) = popFunc list xs ++
						spacing ++ "Pop RegA,\n" ++
					  spacing ++ "Store RegA (Addr " ++ (show (getInt list n)) ++ "),\n"

calcLen :: [Tree] -> Int
calcLen ((DoubloonNode (VarNode _ _ _) (VarNode _ _ _)):xs)	= 2 + calcLen xs
calcLen ((DoubloonNode (VarNode _ _ _) xs):xs')				= 2 + calcLen [xs] + calcLen xs'
calcLen ((BoolNode (VarNode _ _ _) (VarNode _ _ _)):xs) 	= 2 + calcLen xs
calcLen ((AssignNode (VarNode _ _ _) (VarNode _ _ _)):xs) 	= 2 + calcLen xs
calcLen ((AssignNode (VarNode _ _ _) xs):xs')				= 2 + calcLen [xs] + calcLen xs'
calcLen ((OpNode _ (VarNode _ _ _) (VarNode _ _ _)):xs) 	= 4 + calcLen xs 
calcLen ((IfNode _ xs):xs')									= 4 + calcLen xs + calcLen xs'
calcLen ((IfElseNode _ xs t1):xs')							= 4 + calcLen xs + calcLen [t1] + calcLen xs'
calcLen ((WhileNode t1 xs): xs')							= 9 + calcLen xs + calcLen xs'
calcLen ((ForNode t1 t2 t3 xs): xs')						= 10 + calcLen [t1] + calcLen [t2] + calcLen [t3] + calcLen xs + calcLen xs'
calcLen ((ElseNode xs): xs')								= 1 + calcLen xs + calcLen xs'
calcLen ((GiftNode t1): xs)									= 4 + calcLen xs
calcLen ((PrintNode t1): xs)								= 6 + calcLen xs
calcLen ((FuncNode s t1 t2): xs)							= 7 + calcLen t1 + calcLen t2 + calcLen xs
calcLen ((FuncValNode t1 t2): xs) 							= 2 + calcLen xs
calcLen ((VarNode _ _ _): xs)							 	= 2 + calcLen xs
calcLen _													= 0

boolText :: BoolEx -> String
boolText (Comp s t1 t2) = 	(getValue t1) ++ " " ++ s ++ " " ++ getValue(t2)
boolText (Boolean t1)   = 	(getValue t1) 

boolEx :: BoolEx -> [(String, Int)] -> String
boolEx (Comp s t1 t2) list 	= spacing ++ pNode list t2 ++ " RegA,\n" ++ 
							  spacing ++ pNode list t1 ++ " RegB,\n" ++ 
							  spacing ++ "Compute " ++ (getOp s False) ++ " RegB RegA RegA,\n"
boolEx (Boolean t1) list	= spacing ++ pNode list t1 ++ " RegA,\n" ++
							  spacing ++ "Const 1 RegB,\n" ++
							  spacing ++ "Compute NEq RegB RegA RegA,\n"

printNode :: [(String, Int)] -> Tree -> String
printNode list t@(VarNode _ _ _)	= (getValue t) ++ "\n" ++ spacing ++ load list t
printNode list t 				= toSprockell list t ++ spacing ++ "Pop "

pNode :: [(String, Int)] -> Tree -> String
pNode list t@(VarNode _ _ _)	= load list t
pNode list t 				= toSprockell list t ++ spacing ++ "Pop "

load :: [(String, Int)] -> Tree -> String
load list t | getInt list t /= 0 	= "Load (Addr " ++ (show (getInt list t)) ++ ")"
			| otherwise				= "Const " ++ (getValue t)

getOp :: String -> Bool -> String
getOp "+" _ = "Add"
getOp "-" _ = "Sub"
getOp "*" _ = "Mul"
getOp "/" _ = "Div"
getOp "be" False = "NEq"
getOp "be" True  = "Equal"
getOp "below" False = "GtE"
getOp "below" True = "Lt"
getOp "above" False = "LtE"
getOp "above" True = "Gt"
getOp "be below" False = "Gt"
getOp "be below" True = "LtE"
getOp "be above" False = "Lt"
getOp "be above" True = "GtE"

addToList :: [Tree] -> Int -> [(String, Int)]
addToList [] _										= []
addToList ((BootyNode (VarNode s l _) t): xs)	i	= (s, (i+1)): addToList xs (i+1)
addToList ((DoubloonNode (VarNode s l _) t): xs) i 	= (s, (i+1)): addToList xs (i+1)
addToList ((BoolNode (VarNode s l _) t): xs) i		= (s, (i+1)): addToList xs (i+1)
addToList ((TreasureNode (VarNode s l _) t): xs) i 	= (s, (i+1)): addToList xs (i+1)
addToList ((IfNode t1 xs): xs') i 					= list ++ addToList xs' (snd (last list) + 1)
													where
														list = addToList xs i
addToList ((IfElseNode t1 xs t2): xs') i 				= list ++ list' ++ addToList xs' (snd (last list) + 1)
													where
														list = addToList xs i
														list' = addToList [t2] i
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
addToList ((FuncNode s xs xs'): xs'') i 			= (s, i+1):  (list ++ list' ++ addToList xs'' (snd (last list') + 1))
													where
														list = addToList xs (i+1)
														list'= addToList xs' (snd (last list) + 1)
addToList ((FuncValNode t1 t2): xs) i 				= ((getValue t1), (i+1)): addToList xs (i+1)
addToList ((PrintNode xs): xs'') i 					= addToList xs'' (i-1)
addToList _ i 										= []

getInt :: [(String, Int)] -> Tree -> Int
getInt [] _ 							= 0
getInt ((s,i):list) n@(VarNode nS _ _)	| s == nS	= i
										| otherwise = getInt list n
getInt ((s,i):list) n@(FuncNode nS _ _)	| s == nS	= i
										| otherwise = getInt list n
getInt ((s,i):list) n@(DoFuncNode nS _)	| s == nS	= i
										| otherwise = getInt list n
getInt ((s,i):list) n@(FuncValNode r t) | (getValue r) == s = i
										| otherwise = getInt list n
getInt list _ 							= 0

spacing :: String
spacing = "       "