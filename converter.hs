module Converter where

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

{-
	The main function, which starts the convertor.
		Argument 1: The FilePath of the to-be-converted file.
		Returns:	The Monad IO.
-}
start :: FilePath -> IO ()
start input 
	= do
		inHandle <- openFile input ReadMode  
		contents <- hGetContents inHandle
		--putStrLn (contents)
		outHandle <- openFile "out.hs" WriteMode
		if typeAndScopeChecker $ convert $ parse grammar Program $ tokens contents
			then do 
				hPutStr outHandle $ toSprockell [] $ convert $ parse grammar Program $ tokens contents
				hClose inHandle
				hClose outHandle
			else do
				hClose inHandle
				hClose outHandle

{-
	The main converter function.
		Argument 1: A list to keep track of memory locations of variables.
		Argument 2: The Tree to be converted.
		Returns:	A String containing a Haskell File with the Spril instructions.
-}
toSprockell :: [(String, Int)] -> Tree -> String
toSprockell list tree =
	case tree of
		(DoubloonNode t1 t2)-> 	spacing ++ "-- doubloon " ++ (getValue t1) ++ " be " ++ 
								printAndGetNode list t2 ++ " RegA,\n" ++ 
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"
		
		(BoolNode t1 t2)	-> 	spacing ++ "-- order " ++ (getValue t1) ++ " be " ++ 
								printAndGetNode list t2 ++ " RegA,\n" ++ 
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"

		(AssignNode t1 t2) 	-> 	spacing ++ "-- " ++ (getValue t1) ++ " be " ++ 
								printAndGetNode list t2 ++ " RegA,\n" ++
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n"
 
		(OpNode s t1 t2)	->	(getValue t1) ++ " " ++ s ++ " " ++ (getValue t2) ++ "\n" ++
								spacing ++ getNode list t1 ++ " RegB,\n" ++
								spacing ++ getNode list t2 ++ " RegC,\n" ++
								spacing ++ "Compute " ++ (getOp s True) ++ " RegB RegC RegA,\n" ++
								spacing ++ "Push RegA,\n"

		(PrintNode t1) 		->	spacing ++ "-- parrot (" ++ (getValue t1) ++ ")\n"  ++
								spacing ++ getNode list t1 ++ " RegA,\n" ++
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
								spacing ++ getNode list t1 ++ " RegA,\n" ++
								spacing ++ "Const 1 RegB,\n" ++
								spacing ++ "Compute Add RegA RegB RegA,\n" ++ 
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n" ++ 
								spacing ++ "Push RegA,\n"

		(PlunderNode t1)	-> 	spacing ++ "-- plunder " ++ (getValue t1) ++ "\n" ++
								spacing ++ getNode list t1 ++ " RegA,\n" ++
								spacing ++ "Const 1 RegB,\n" ++
								spacing ++ "Compute Sub RegA RegB RegA,\n" ++ 
								spacing ++ "Store RegA (Addr " ++ (show (getInt list t1)) ++ "),\n" ++
								spacing ++ "Push RegA,\n"

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
												spacing ++ "Branch RegA (Rel(" ++ (show ((calcLen xs)+6)) ++ ")),\n" ++
												(concat (map (toSprockell list) xs)) ++
												spacing ++ "Pop RegA,\n" ++
												spacing ++ "Pop RegE,\n" ++
												spacing ++ "Push RegA,\n" ++
												spacing ++ "Jump (Ind RegE),\n" ++
												spacing ++ "Pop RegE,\n" 

		(ForNode t0 (BoolExNode t1) t3 xs)	-> 	spacing ++ "-- navigate("++ (getValue t0) ++ ". " ++ boolText t1 ++ ". " ++ getValue(t3) ++ ")\n" ++
												toSprockell list t0 ++
												spacing ++ "Compute Add PC Zero RegE,\n" ++
												spacing ++ "Push RegE,\n" ++ 
												boolEx t1 list ++
												spacing ++ "Branch RegA (Rel(" ++ (show ((calcLen xs)+(calcLen [t3])+6)) ++ ")),\n" ++
												(concat (map (toSprockell list) xs)) ++ 
												toSprockell list t3 ++
												spacing ++ "Pop RegA,\n" ++
												spacing ++ "Pop RegE,\n" ++
												spacing ++ "Push RegA,\n" ++
												spacing ++ "Jump (Ind RegE),\n" ++
												spacing ++ "Pop RegE,\n" 

		n@(ReturnNode _ t1)					->	spacing ++ "-- avast " ++ (getValue t1) ++ "\n" ++
												spacing ++ "Pop RegE,\n" ++
												spacing ++ getNode list t1 ++ " RegA,\n" ++
												spacing ++ "Push RegA,\n" ++
												spacing ++ "Push RegE,\n"

		n@(FuncNode "flagship" xs xs')		-> 	spacing ++ "-- flagship()\n" ++
												(concat (map (toSprockell list) xs'))

		n@(IntFuncNode s xs xs')			->	spacing ++ "-- " ++ s ++ "(" ++ (funcText xs) ++ ")\n" ++
												spacing ++ "Const 3 RegA,\n" ++
												spacing ++ "Compute Add PC RegA RegE,\n" ++ 
												spacing ++ "Store RegE (Addr " ++ (show (getInt list n)) ++ "),\n" ++ 
												spacing ++ "Jump (Rel(" ++ (show ((calcLen xs')+(calcLen xs)+3)) ++ ")),\n" ++
												popFunc list xs ++ 
												(concat (map (toSprockell list) xs')) ++
												spacing ++ "Pop RegE,\n" ++
												spacing ++ "Jump (Ind RegE),\n"

		n@(DoFuncNode s xs)					-> 	spacing ++ "-- " ++ s ++ "(" ++ (funcText xs) ++ ")\n" ++
												spacing ++ "Const " ++ (show ((calcLen xs)+4)) ++ " RegA,\n" ++ 
												spacing ++ "Compute Add PC RegA RegE,\n" ++
												spacing ++ "Push RegE,\n" ++
												pushFunc list xs ++ 
												spacing ++ "Load (Addr " ++ (show (getInt list n)) ++ ") RegA,\n" ++ 
												spacing ++ "Jump (Ind RegA),\n"

		_					-> 	""

{-
	Helper Function of toSprockell - Prints a single Char to Standard Out.
		Argument 1: The list of memory locations.
		Argument 2: The char to print.
		Returns:	String of Spril Instructions instructing a Print function.
-}
printFunc :: [(String, Int)] -> Char -> String
printFunc list c =	spacing ++ "Const (ord '" ++ [c] ++ "') RegA,\n" ++
					spacing ++ "Const (ord '0') RegB,\n" ++ 
					spacing ++ "Compute Add RegA RegB RegB,\n" ++
   					spacing ++ "Write RegB stdio,\n" ++
   					spacing ++ "Read (Addr 0x0),\n" ++
   					spacing ++ "Receive RegB,\n"

{-
	Helper Function of toSprockell - Stores an Array to memory.
		Argument 1: The Identifier of the Array.
		Argument 2: The List of Array elements.
		Argument 3: The List of memory locations.
		Returns:	String of Spril Intructions Instructing to store an Array.
-}
storeArray :: Tree -> [Tree] -> [(String, Int)] -> String
storeArray idf (x:xs) list 	= spacing ++ getNode list x

{-
	Helper Function of toSprockell - Gets the String representation of a Function.
		Argument 1: The Arguments of a Function to get the String representation from.
		Returns: 	The String representation of the Function Arguments
-}
funcText :: [Tree] -> String
funcText []     = ""
funcText [x]    = (getValue x)
funcText (x:xs) = (getValue x) ++ ", " ++ funcText xs

{-
	Helper Function of toSprockell - Pushes Function Arguments to the Stack.
		Argument 1: The list of memory locations.
		Argument 2: The List of Function Arguments.
		Returns:	String of Spril Instructions instructing to push Function Arguments.
-}
pushFunc :: [(String, Int)] -> [Tree] -> String
pushFunc list []  	 =  "" 
pushFunc list (x:xs) = 	spacing ++ "Const " ++ (show (getInt list x)) ++ " RegA,\n" ++ 
						spacing ++ "Push RegA,\n" ++
						pushFunc list xs

{-
	Helper Function of toSprockell - Pops Function Arguments from the Stack.
		Argument 1: The list of memory locations.
		Argument 2: The List of Function Arguments.
		Returns:	String of Spril Instructions instructing to pop Function Arguments.
-}
popFunc :: [(String, Int)] -> [Tree] -> String
popFunc list []  	= "" 
popFunc list (n:xs) = popFunc list xs ++
					  spacing ++ "Pop RegA,\n" ++
					  spacing ++ "Load (Deref RegA) RegB,\n" ++ 
					  spacing ++ "Store RegB (Addr " ++ (show (getInt list n)) ++ "),\n"

{-
	Helper Function of toSprockell - Calculates the number of Spril-lines of a certain segment.
		Argument 1: The List of Trees to calculate the number of Spril-lines from.
		Returns:	The number of Spril-Lines needed to convert the given segement.
-}
calcLen :: [Tree] -> Int
calcLen ((DoubloonNode (VarNode _ _ _) (VarNode _ _ _)):xs)	= 2 + calcLen xs
calcLen ((DoubloonNode (VarNode _ _ _) xs):xs')				= 2 + calcLen [xs] + calcLen xs'
calcLen ((BoolNode (VarNode _ _ _) (VarNode _ _ _)):xs) 	= 2 + calcLen xs
calcLen ((AssignNode (VarNode _ _ _) (VarNode _ _ _)):xs) 	= 2 + calcLen xs
calcLen ((AssignNode (VarNode _ _ _) xs):xs')				= 2 + calcLen [xs] + calcLen xs'
calcLen ((OpNode _ (VarNode _ _ _) (VarNode _ _ _)):xs) 	= 4 + calcLen xs 
calcLen ((OpNode _ (VarNode _ _ _) xs):xs') 				= 4 + calcLen [xs] + calcLen xs' 
calcLen ((IfNode _ xs):xs')									= 4 + calcLen xs + calcLen xs'
calcLen ((IfElseNode _ xs t1):xs')							= 4 + calcLen xs + calcLen [t1] + calcLen xs'
calcLen ((WhileNode t1 xs): xs')							= 9 + calcLen xs + calcLen xs'
calcLen ((ForNode t1 t2 t3 xs): xs')						= 10 + calcLen [t1] + calcLen [t2] + calcLen [t3] + calcLen xs + calcLen xs'
calcLen ((ElseNode xs): xs')								= 1 + calcLen xs + calcLen xs'
calcLen ((GiftNode t1): xs)									= 5 + calcLen xs
calcLen ((PlunderNode t1): xs)								= 5 + calcLen xs
calcLen ((PrintNode (VarNode _ _ _)): xs)					= 6 + calcLen xs
calcLen ((PrintNode t1): xs)								= 6 + calcLen [t1] + calcLen xs
calcLen ((IntFuncNode _ t1 t2): xs)							= 6 + calcLen t1 + calcLen t2 + calcLen xs
calcLen ((FuncValNode t1 t2): xs) 							= 3 + calcLen xs
calcLen ((DoFuncNode _ xs): xs')							= 5 + calcLen xs + calcLen xs'
calcLen ((VarNode _ _ _): xs)							 	= 2 + calcLen xs
calcLen ((ReturnNode _ _): xs)								= 4 + calcLen xs
calcLen _													= 0

{-
	Helper Function of toSprockell - Converts a Boolean Expression ('Aye' or 'n be below 5') to a String representation.
		Argument 1: The Boolean Expression to convert to a String.
		Returns:	The String representation of the Boolean Expression.
-}
boolText :: BoolEx -> String
boolText (Comp s t1 t2) = 	(getValue t1) ++ " " ++ s ++ " " ++ getValue(t2)
boolText (Boolean t1)   = 	(getValue t1) 

{-
	Helper Function of toSprockell - Evalueates a Boolean Expression.
		Argument 1: The Boolean Expression to evaluate.
		Argument 2: The List of memory locations.
		Returns:	String of Spril Instructions instruction to evaluate a Boolean Expression.
-}
boolEx :: BoolEx -> [(String, Int)] -> String
boolEx (Comp s t1 t2) list 	= spacing ++ getNode list t2 ++ " RegA,\n" ++ 
							  spacing ++ getNode list t1 ++ " RegB,\n" ++ 
							  spacing ++ "Compute " ++ (getOp s False) ++ " RegB RegA RegA,\n"
boolEx (Boolean t1) list	= spacing ++ getNode list t1 ++ " RegA,\n" ++
							  spacing ++ "Const 1 RegB,\n" ++
							  spacing ++ "Compute Equal RegB RegA RegA,\n"

{-
	Helper Function of toSprockell - Prints the node and gets it from the Stack of from Memory.
		Argument 1: The list of memory locations.
		Argument 2: The Tree to print and get.
		Result:		String of Spril Instructions intruction to load a variable from the Stack or from Memory.
-}
printAndGetNode :: [(String, Int)] -> Tree -> String
printAndGetNode list t@(VarNode _ _ _)	= (getValue t) ++ "\n" ++ spacing ++ load list t
printAndGetNode list t 					= toSprockell list t ++ spacing ++ "Pop "

{-
	Helper Function of toSprockell - Gets a node from the Stack of from Memory.
		Argument 1: The list of memory locations.
		Argument 2: The Tree to get.
		Result:		String of Spril Instructions intruction to load a variable from the Stack or from Memory.
-}
getNode :: [(String, Int)] -> Tree -> String
getNode list t@(VarNode _ _ _)	= load list t
getNode list t 					= trace (show t) $ toSprockell list t ++ spacing ++ "Pop "

{-
	Helper Function of toSprockell - Loads a Indetifier from Memory or loads a Constant.
		Argument 1: The list of Memory Locations.
		Argument 2: The Tree to load.
		Returns:	String of Spril Instructions instructing to load a Identifier from Memory or load a Constant.
-}
load :: [(String, Int)] -> Tree -> String
load list t | getInt list t /= 0 	= "Load (Addr " ++ (show (getInt list t)) ++ ")"
			| otherwise				= "Const (" ++ (getValue t) ++ ")"

{-
	Helper Function of toSprockell - Converts a String Operator to a String Operator Spril can use.
		Argument 1: The String to convert.
		Argument 2: True = Normal, False = Reversed (Needed for the while-loop)
		Returns:	The String with the converted Operator.
-}
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

{-
	Helper Function of toSprockell - Adds all Variables to the List of Memory locations, given them a memory location.
		Argument 1: The list of Trees with the variables.
		Argument 2: The last memory location. (Can be 0).
		Returns:	List of memory locations.
-}
addToList :: [Tree] -> Int -> [(String, Int)]
addToList [] _										= []
addToList ((BootyNode (VarNode s l _) t): xs)	i	= (s, (i+1)): addToList xs (i+1)
addToList ((DoubloonNode (VarNode s l _) t): xs) i 	= (s, (i+1)): addToList xs (i+1)
addToList ((BoolNode (VarNode s l _) t): xs) i		= (s, (i+1)): addToList xs (i+1)
addToList ((ArrayNode _ (VarNode s _ _) t): xs) i 	= (s, (i+1)): addToList xs (i+1+(length t))
addToList ((IfNode t1 xs): xs') i 					= list ++ addToList xs' (getNew list)
													where
														list = addToList xs i
addToList ((IfElseNode t1 xs t2): xs') i 			= list ++ list' ++ addToList xs' (getNew (list ++ list'))
													where
														list = addToList xs i
														list' = addToList [t2] i
addToList ((ElseNode xs): xs') i 					= list ++ addToList xs' (getNew list)
													where
														list = addToList xs i
addToList ((WhileNode t1 xs): xs') i 				= list ++ addToList xs' (getNew list)
													where
														list = addToList xs i
addToList ((ForNode t1 _ _ xs): xs') i 				= list' ++ list ++ addToList xs' (getNew (list' ++ list))
													where
														list'= addToList [t1] i
														list = addToList xs (getNew list)
addToList ((FuncNode s xs xs'): xs'') i 			= (s, i+1):  (list ++ list' ++ addToList xs'' (getNew (list ++ list')))
													where
														list = addToList xs (i+1)
														list'= addToList xs' (getNew list)
addToList ((IntFuncNode s xs xs'): xs'') i 			= (s, i+1):  (list ++ list' ++ addToList xs'' (getNew (list ++ list')))
													where
														list = addToList xs (i+1)
														list'= addToList xs' (getNew list)
addToList ((FuncValNode t1 t2): xs) i 				= ((getValue t1), (i+1)): addToList xs (i+1)
addToList ((PrintNode xs): xs'') i 					= addToList xs'' (i-1)
addToList ((ReturnNode _ _): xs) i 					= addToList xs i
addToList _ i 										= []

{-
	Helper Function of addToList - Gets a free memory location from the list.
		Argument 1: The current list of memory locations.
		Returns:	A free spot in the list.
-}
getNew :: [(String, Int)] -> Int
getNew [] 	= 1
getNew list = (snd (last list)) + 1 

{-
	Helper Function of toSprockell - Gets a memory location from the list.
		Argument 1: The list of memory locations.
		Argument 2: The Tree ot get the memory location from.
		Returns:	The memory location of the Tree.
-}
getInt :: [(String, Int)] -> Tree -> Int
getInt [] _ 							= 0
getInt ((s,i):list) n@(VarNode nS _ _)	| s == nS	= i
										| otherwise = getInt list n
getInt ((s,i):list) n@(IntFuncNode nS _ _)	| s == nS	= i
										| otherwise = getInt list n
getInt ((s,i):list) n@(DoFuncNode nS _)	| s == nS	= i
										| otherwise = getInt list n
getInt ((s,i):list) n@(FuncValNode r t) | (getValue r) == s = i
										| otherwise = getInt list n
getInt list _ 							= 0

{-
	Helper Function of toSprockell - A shortcut to add spacing in front of every line for readability.
		Returns:	A String of spaces.
-}
spacing :: String
spacing = "       "