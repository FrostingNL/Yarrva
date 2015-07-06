module Checker where

import Data.Char
import FPPrac.Trees
import Data.List
import Debug.Trace
import Grammar

--main = do putStr (show (typeAndScopeChecker $ convert test1))

{-
	The Main function of this checker file. It calls typechecker and scopechecker on the specified Tree.
		Argument 1: The Tree to check.
		Returns:	True if the checks succeeded, False if not.
-}
typeAndScopeChecker :: Tree -> Bool
typeAndScopeChecker node = typeChecker [] node && inScope [] node

{-
	The function to check if a certain variable is used further on in the program. 
		Argument 1: The variable to search for
		Argument 2: The Tree to search in
		Returns:	True if all variables are used after their declaration, False if not.
-}
checkUsage :: (String, Types) -> Tree -> Bool
checkUsage t@(s,p) tree = 
	case tree of
		(VarNode s2 l c)			-> s == s2
		(BootyNode t1 t2) 			-> usage t t2
		(DoubloonNode t1 t2) 		-> usage t t2
		(BoolNode t1 t2)			-> usage t t2
		(ArrayNode t1 t2 xs)		-> uMap t xs
		(AssignNode t1 t2) 			-> usage t t1 || usage t t2
		(ArrayOpNode t1 t2)			-> usage t t1 || usage t t2
		(OpNode _ t1 t2)			-> usage t t1 || usage t t2
		(BoolExNode (Comp _ t1 t2))	-> usage t t1 || usage t t2
		(BoolExNode (Boolean t1))	-> usage t t1
		(GiftNode t1)				-> usage t t1
		(PlunderNode t1)			-> usage t t1
		(FuncValNode t1 t2)			-> usage t t1
		(PrintNode t1)				-> usage t t1
		(ReturnNode _ t1)			-> usage t t1
		(DoFuncNode s2 xs)			-> s == s2 || uMap t xs
		(IfNode t1 xs) 				| usage t t1 || uMap t xs				-> True
									| otherwise 							-> False
		(IfElseNode t1 xs xs') 		| usage t t1 || uMap t xs || usage t xs'-> True
									| otherwise 							-> False
		(ElseNode xs)				| uMap t xs								-> True
									| otherwise 							-> False
		(ForNode t1 t2 t3 xs)  		| usage t t1 || usage t t2 || uMap t xs	-> True
									| otherwise 							-> False
		(WhileNode t1 xs)			| usage t t1 || uMap t xs				-> True
									| otherwise 							-> False
		(FuncNode s' xs' xs) 		| uMap t xs && falseMap t xs' 			-> True
									| otherwise 							-> False
		(IntFuncNode s' xs' xs)		| uMap t xs && falseMap t xs' 			-> True
									| otherwise 							-> False
		(BoolFuncNode s' xs' xs)	| uMap t xs && falseMap t xs' 			-> True
									| otherwise 							-> False							
		(ZupaNode _ xs)				| uMap t xs								-> True
									| otherwise 							-> warning s
{-
	Helper functions for checkUsage.
-}
warning s	= trace ("*** Warning: '" ++ s ++ "' is never used.") True
usage a b	= checkUsage a b
uMap a b	= or (map (checkUsage a) b)
falseMap a b= all (==False) (map (checkUsage a) b)

{-
	The function which checks if all types of a given Tree are consistent and correct.
		Argument 1: A list with the current Scope. Can be empty as it gets filled during execution.
		Argument 2: The Tree to check the Types of.
		Returns: 	True if every type is correct and consistent, False if not.
-}
typeChecker :: [[(String, Types)]] -> Tree -> Bool
typeChecker list tree = 
	case tree of
		n@(OpNode "+" t1 t2)				-> checkType n  Int list || checkType n Str list
		n@(OpNode s t1 t2)					-> checkType n  Int list
		n@(BoolExNode t1)					-> checkType n  Boo list
		(GiftNode t1)						-> checkType t1 Int list
		(PlunderNode t1)					-> checkType t1 Int list
		(AssignNode t1 t2) 					-> checkAssignType t1 t2 list
		(ElseNode xs)						-> tCheckerMap xs list
		(ZupaNode s xs) 					-> tCheckerMap xs list 
		(FuncNode s xs xs')					-> tCheckerMap xs' list && tMap list xs
		(IntFuncNode s xs xs')				-> tCheckerMap2 xs' xs list
		(BoolFuncNode s xs xs')				-> tCheckerMap2 xs' xs list
		(IfNode t1 xs)						-> tCheckerMap xs list && checkType t1 Boo list
		(IfElseNode t1 xs xs')				-> tCheckerMap xs list && typeChecker list xs' && checkType t1 Boo list
		(WhileNode t1 xs)					-> tCheckerMap xs list && checkType t1 Boo list
		(ForNode t1 t2 t3 xs)				-> typeChecker newList t2 && typeChecker newList t3 && tCheckerMap xs newList
											where
												newList = (addToScope [t1] (concat list)): list
		n@(FuncValNode t1 t2)				-> addToScope [n] [] /= []
		_ 									-> True
{-
	Helper functions for typeChecker.
-}
tMap a b 		= and (map (typeChecker a) b)
tCheckerMap a b = tMap ((addToScope a (concat b)): b) (getOtherNodes a)
tCheckerMap2 a c b = tMap ((addToScope (c ++ a) (concat b)): b) (getOtherNodes a)

{-
	The function which gets every node except declaration nodes. This function is used in the creation of the scope.
		Argument 1: A list of Trees to get the nodes from.
		Returns:	A list of Trees without declaration nodes.
-}
getOtherNodes :: [Tree] -> [Tree]
getOtherNodes []						= []
getOtherNodes ((BootyNode _ _): xs) 	= getOtherNodes xs
getOtherNodes ((DoubloonNode _ _): xs)	= getOtherNodes xs
getOtherNodes ((BoolNode _ _): xs)		= getOtherNodes xs
getOtherNodes ((ArrayNode _ _ _): xs)  	= getOtherNodes xs
getOtherNodes (n:xs)					= n: getOtherNodes xs

{-
	The function which creates ONE scope. This scope is created for a list of Trees, i.e. a body of a function without going deeper.
		Argument 1: A list of Trees from which to create the scope.
		Argument 2: Previous scopes concatenated, can be empty.
		Returns: 	A new scope for the given list of Trees.
-}
addToScope :: [Tree] -> [(String, Types)] -> [(String, Types)]
addToScope [] _										= []
addToScope ((BootyNode (VarNode s l c) t): xs) li	| checkType t Str [li] && inList s li		= error ("Declaration: " ++ s ++ " has already been declared! Line:" ++ (show l) ++ ":" ++ (show c))
													| checkType t Str [li] 						= (s, Str): addToScope xs (li ++ [(s,Str)])
													| otherwise 								= incType t "String" l c 
addToScope ((DoubloonNode (VarNode s l c) t): xs) li| checkType t Int [li] && inList s li		= error ("Declaration: " ++ s ++ " has already been declared! Line:" ++ (show l) ++ ":" ++ (show c))
													| checkType t Int [li]						= (s, Int): addToScope xs (li ++ [(s,Int)])
													| otherwise 								= incType t "Integer" l c
addToScope ((BoolNode (VarNode s l c) t): xs) li	| checkType t Boo [li] && inList s li		= error ("Declaration: " ++ s ++ " has already been declared! Line:" ++ (show l) ++ ":" ++ (show c))
													| checkType t Boo [li]						= (s, Boo): addToScope xs (li ++ [(s,Boo)])
													| otherwise 								= incType t "Boolean" l c
addToScope ((ArrayNode (VarNode s l c) t@(VarNode s2 l2 c2) xs): xs') li
													| bool && inList s li						= error ("Declaration: " ++ s ++ " has already been declared! Line:" ++ (show l) ++ ":" ++ (show c))	
													| bool 										= (s2, Arr typ): addToScope xs' (li ++ [(s2,Arr typ)])
													| otherwise 								= incType2 t s l2 c2
													where
														bool = and $ map (\x -> checkType x typ [li]) xs
														typ = (getTypeFromString s)
addToScope ((FuncValNode (VarNode s l c) (VarNode s2 l2 c2)): xs) list
													= (s, (getTypeFromString s2)): addToScope xs (list ++ [(s,(getTypeFromString s2))]) 
addToScope ((IntFuncNode s _ _): xs) l				= (s, Int): addToScope xs (l ++ [(s,Int)])
addToScope ((BoolFuncNode s _ _): xs) l				= (s, Boo): addToScope xs (l ++ [(s,Boo)])
addToScope (_: xs) l								= addToScope xs l
{-
	Helper functions for addToScope.
-}
incType a s l c  = error ("Incorrect Type: " ++ (getValue a) ++ " is not a " ++ s ++ " ! Line:" ++ (show l) ++ ":" ++ (show c))
incType2 a s l c = error ("Incorrect Type: Not all values of " ++ (getValue a) ++ " are " ++ s ++ "s ! Line:" ++ (show l) ++ ":" ++ (show c))
inList :: String -> [(String, Types)] -> Bool
inList a b = (elem (a,Int) b) || (elem (a,Str) b) || (elem (a,Boo) b) || (elem (a,Arr Int) b) || (elem (a,Arr Boo) b) || (elem (a,Arr Str) b) || (elem (a,Arr Str) b)

{-
	Checks wether the type of the Right Hand Side of an Assignments is the same as the Left Hand Side.
-}
checkAssignType :: Tree -> Tree -> [[(String, Types)]] -> Bool
checkAssignType idf val list = checkType val (getTreeType idf list) list

{-
	Converts a String into a Type
-}
getTypeFromString :: String -> Types
getTypeFromString s
	| s == "String" || s == "booty"  	= Str
	| s == "Int" 	|| s == "doubloon"	= Int
	| s == "Bool" 	|| s == "order"		= Boo
	| s == "Array"	= Arr Err
	| otherwise		= Err

{-
	Converts a Type into a String
-}
getStringFromType :: Types -> String
getStringFromType s
	| s == Str 		= "String" 
	| s == Int 		= "Int"
	| s == Boo 		= "Bool"
	| otherwise		= "Error"

{-
	The function which checks if a certain Tree is of a certain Type.
		Argument 1: The Tree to check.
		Argument 2: The Type to check.
		Argument 3: The current Scope, in order to get Types of Identifiers
		Returns:	True if the Type is correct, an Error if it's incorrect.
-}
checkType :: Tree -> Types -> [[(String, Types)]] -> Bool
checkType tree Int list =
	case tree of 
		n@(VarNode s l c)  					| and (map (isNumber) s) || typ == Int || typ == Arr Int 	-> True
											| otherwise													-> wrongType n "Integer" l c 
											where
												typ = getType s l c list
		(OpNode s t1 t2)					-> checkType t1 Int list && checkType t2 Int list 
		(DoFuncNode s xs)					-> getType s 0 0 list == Int
		(ArrayOpNode t1 t2)					-> checkType t1 Int list && checkType t2 Int list 		
		(GiftNode t1)						-> checkType t1 Int list
		(PlunderNode t1)					-> checkType t1 Int list
		_									-> False

checkType tree Str list =
	case tree of
		n@(VarNode s l c) 					| isString s || typ == Str || typ == Arr Str 	-> True
											| otherwise										-> wrongType n "String" l c
											where
												typ = getType s l c list
		(OpNode "+" t1 t2)					-> checkType t1 Str list && checkType t2 Str list 
		(DoFuncNode s xs)					-> getType s 0 0 list == Str
		(ArrayOpNode t1 t2)					-> checkType t1 Str list && checkType t2 Str list 		
		_									-> False

checkType tree Boo list =
	case tree of
		n@(VarNode s l c)	 				| s == "Aye" || s == "Nay" || t == Boo || t == Arr Boo	-> True
											| otherwise												-> wrongType n "Boolean" l c
											where
												t = getType s l c list
		(BoolExNode (Comp "be" t1 t2))  	-> checkType t1 t list && checkType t2 t list
											where
												t = getTreeType t1 list
		(BoolExNode (Boolean t1))			| checkType t1 Boo list									-> True
											| otherwise												-> wrongType t1 "Boolean" (getL t1) (getC t1)
		(BoolExNode (Comp _ t1 t2)) 		-> checkType t1 Int list && checkType t1 Int list
		(DoFuncNode s xs)					-> getType s 0 0 list == Boo
		(ArrayOpNode t1 t2)					-> checkType t1 Boo list && checkType t2 Boo list 	
		_									-> False

checkType tree (Arr t) list =
	case tree of
		n@(VarNode s l c) 					| isString s || typ == Arr t 	-> True
											| otherwise						-> wrongType n "String" l c
											where
												typ = getType s l c list
		(OpNode "+" t1 t2)					-> checkType t1 t list && checkType t2 t list 
		(DoFuncNode s xs)					-> getType s 0 0 list == t
		(ArrayOpNode t1 t2)					-> checkType t1 t list && checkType t2 t list 		
		_									-> False
{-
	Helper Function for checkType.
-}
wrongType a b c d	= error ("Incorrect Type: " ++ (getS a) ++ " is not an " ++ b ++ "! Line: " ++ (show c) ++ ":" ++ (show d))
getL (VarNode _ l _) = l
getL _ 				 = 0 
getC (VarNode _ _ c) = c
getC _ 				 = 0 
getS (VarNode s _ _) = s
getS _				 = ""

{-
	Gets a Type from a certain String (Identifier).
		Argument 1: The String to get the Type from.
		Argument 2: The line number in which the String is.
		Argument 3: The column number on which the String starts.
		Argument 4: The current Scope.
		Returns: 	The Type of the given String, or an Error if the String isn't in the current Scope.
-}
getType :: String -> Int -> Int -> [[(String, Types)]] -> Types
getType	s l c []					| isString s 				= Str 
									| and (map (isNumber) s) 	= Int
									| isBoolean s 				= Boo 
									| isArray s 				= Arr Err
									| otherwise																= error ("Declaration: '" ++ s ++ "' has not been defined yet! Line: " ++ (show l) ++ ":" ++ (show c))
getType s l c ([]:list)				= getType s l c list
getType s l c (((s2,t):tup):list) 	| s == s2 	= t
							  		| otherwise = getType s l c (tup:list)

{-
	Gets a Type from a certain Tree
		Argument 1: The Tree to get the Type from.
		Argument 2: The current Scope.
		Returns: 	The Type of the given Tree, or an Error if the Tree isn't in the current Scope.
-}
getTreeType :: Tree -> [[(String, Types)]] -> Types
getTreeType (VarNode s l c) list 		= getType s l c list
getTreeType (OpNode s t1 t2) list 		= getTreeType t1 list
getTreeType _ list 			 			= Err

{-
	Checks wether a certain Tree is inside the given Scope.
		Argument 1: The scope to check.
		Argument 2: The Tree to check.
		Returns: 	True if the Tree is in the current Scope, Error if it isn't.
-}
inScope :: [[(String, Types)]] -> Tree -> Bool
inScope ([]:x:list) a 							= inScope (x:list) a
inScope [[]] (VarNode s _ _)					= isString s || isNumber (head s) || (head s) == '-' || isBoolean s
inScope (((s2,_):tup):list) n@(VarNode s l c) 	| s == s2 || isString s || isNumber (head s) || (head s) == '-' || isBoolean s	= True
												| otherwise = inScope (tup:list) n
inScope (((s2,_):tup):list) n@(DoFuncNode s _) 	| s == s2 = True
												| otherwise = inScope (tup:list) n
inScope list tree = 
	case tree of
		(BoolExNode (Boolean t1)) 	| inScope list t1 -> True
									| otherwise 	  -> scopeError (getValue t1)
		(GiftNode t1) 				| inScope list t1 -> True
									| otherwise 	  -> scopeError (getValue t1)
		(PlunderNode t1) 			| inScope list t1 -> True
									| otherwise 	  -> scopeError (getValue t1)
		(PrintNode t1)				| inScope list t1 -> True
									| otherwise 	  -> scopeError (getValue t1)
		(ReturnNode s t1)			| inScope list t1 -> True
									| otherwise 	  -> scopeError (getValue t1)
		(BootyNode t1 t2)			| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 	  		-> True
		(DoubloonNode t1 t2) 		| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 	  		-> True
		(BoolNode t1 t2) 			| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 	  		-> True
		(AssignNode t1 t2)			| not $ inScope list t1 -> scopeError (getValue t1) 
									| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 	  		-> True
		(OpNode _ t1 t2) 			| not $ inScope list t1 -> scopeError (getValue t1) 
									| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 	 	 	-> True
		(BoolExNode (Comp _ t1 t2)) | not $ inScope list t1 -> scopeError (getValue t1) 
									| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 	 		-> True
		(FuncValNode t1 t2)			| not $ inScope list t1 -> scopeError (getValue t1) 
									| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 	 		-> True
		(ForNode t1 t2 t3 xs)		| not $ inScope list t1	   	-> scopeError (getValue t1) 
									| not $ inScope newList t2 	-> scopeError (getValue t2)
									| not $ inScope newList t3 	-> scopeError (getValue t3)
									| otherwise				   	-> scopeM xs newList
									where
										newList = (addToScopeSC [t1]): list 
		(IfNode t1 xs)				| not $ inScope list t1 -> scopeError (getValue t1)
									| otherwise 			-> scopeM xs list
		(IfElseNode t1 xs t2)		| not $ inScope list t1 -> scopeError (getValue t1)
									| not $ inScope list t2 -> scopeError (getValue t2)
									| otherwise 			-> scopeM xs list
		(WhileNode t1 xs)			| not $ inScope list t1 -> scopeError (getValue t1) 
									| otherwise 			-> scopeM xs list 
		(DoFuncNode s xs)			| not $ stringInScope s list -> scopeError s
									| otherwise 				 -> scopeM2 list xs
		(FuncNode s xs xs')			-> funcM xs xs' list 
		(ArrayNode t1 t2 xs)		-> scopeM2 list xs 
		(ElseNode xs)				-> scopeM xs list 
		n@(ZupaNode s xs)			-> scopeM xs list  && usageM n n
		(IntFuncNode s xs xs')		-> funcM xs xs' list
		(BoolFuncNode s xs xs')		-> funcM xs xs' list
		_							-> False
{-
	Helper Functions for inScope
-}
usageM a b	= and (map (\x -> checkUsage x a) (addandoScope b))
scopeM a b 	= scopeM2 ((addToScopeSC a): b) (getOtherNodes a)
funcM a b c = scopeM2 (((addToScopeSC a) ++ (addToScopeSC b)): c) b
scopeM2 a b = and (map (inScope a) b)
scopeError a = error ("Scope: " ++ (show a) ++ " is not in the current Scope!")
 
 {-
	Creates a new Scope when going down a Level.
		Argument 1: The starting Tree.
		Result:		The new Scope.
-}
addandoScope :: Tree -> [(String, Types)]
addandoScope tree =
	case tree of 
		(IfNode _ xs)		 -> (addToScopeSC xs) ++ (concat (map addandoScope xs))
		(IfElseNode _ xs xs')-> (addToScopeSC xs) ++ (concat (map addandoScope xs) ++ addandoScope xs')
		(ElseNode xs)		 -> (addToScopeSC xs) ++ (concat (map addandoScope xs))
		(WhileNode _ xs)	 -> (addToScopeSC xs) ++ (concat (map addandoScope xs))
		(ZupaNode _ xs)		 -> (addToScopeSC xs) ++ (concat (map addandoScope xs))
		(ForNode xs _ _ xs') -> (addToScopeSC [xs]) ++ (addToScopeSC xs') ++ (concat (map addandoScope xs'))
		(FuncNode _ xs xs')  -> (addToScopeSC xs) ++ (addToScopeSC xs') ++ (concat (map addandoScope xs'))
		_					 -> []

{-
	A variation on addToScope.
		Argument 1: The list of Trees to add to the Scope.
		Returns:	ONE scope.
-}	
addToScopeSC :: [Tree] -> [(String, Types)]
addToScopeSC [] 										= []
addToScopeSC ((BootyNode (VarNode s l c) t): xs) 		= (s, Str): addToScopeSC xs 
addToScopeSC ((DoubloonNode (VarNode s l c) t): xs) 	= (s, Int): addToScopeSC xs 
addToScopeSC ((BoolNode (VarNode s l c) t): xs)			= (s, Boo): addToScopeSC xs
addToScopeSC ((ArrayNode (VarNode s2 _ _) (VarNode s l c) v): xs) 		
														= (s, Arr (getTypeFromString s2)): addToScopeSC xs
addToScopeSC ((FuncValNode (VarNode s l c) (VarNode s2 l2 c2)): xs)
														= (s, (getTypeFromString s2)): addToScopeSC xs 
addToScopeSC ((IntFuncNode s _ _): xs) 					= (s, Int): addToScopeSC xs 
addToScopeSC ((BoolFuncNode s _ _): xs) 				= (s, Boo): addToScopeSC xs
addToScopeSC (_: xs) 									= addToScopeSC xs 

{-
	Gets the String value of a given Tree.
		Argument 1: The Tree to get the value from.
		Returns: 	The String value of the given Tree.
-}
getValue :: Tree -> String
getValue (VarNode s _ _) 	| s == "Aye" = "1"
					   		| s == "Nay" = "0"
							| otherwise	= s
getValue (OpNode s _ _) = s
getValue (FuncNode s _ _) = s
getValue (DoubloonNode t1 t2) = "doubloon " ++ getValue t1 ++ " be " ++ getValue t2
getValue (FuncValNode t1 t2) = getValue t1 ++ " " ++ getValue t2
getValue (GiftNode t1) = "gift " ++ getValue t1
getValue (DoFuncNode s xs) = s ++ "(" ++ (concat (map getValue xs)) ++ ")"
getValue (IntFuncNode s _ _) = s
getValue (BoolFuncNode s _ _) = s
getValue (BoolExNode (Comp s t1 t2)) = getValue t1 ++ " " ++ s ++ " " ++ getValue t2
getValue s = show s 

{-
	Checks if a String is in the current Scope.
		Argument 1: The String to check.
		Argument 2: The current Scope.
		Returns: 	True if the String is in the Scope, False if not.
-}
stringInScope :: String -> [[(String, Types)]] -> Bool
stringInScope s [] = False
stringInScope s ([]: rest) = stringInScope s rest
stringInScope s (((s2,_): list): rest) = s == s2 || stringInScope s (list: rest)