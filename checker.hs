import Data.Char
import FPPrac.Trees
import Data.List
import Debug.Trace
import Grammar

typeAndScopeChecker :: Tree -> Bool
typeAndScopeChecker node = typeChecker [] node && isAccessible [] node

doUsage :: Tree -> (String, Types) -> Bool
doUsage node a = checkUsage a node

checkUsage :: (String, Types) -> Tree -> Bool
checkUsage (s,t) (VarNode s2 l)					= s == s2
checkUsage (s,t) (BootyNode t1 t2)				= checkUsage (s,t) t2
checkUsage (s,t) (DoubloonNode t1 t2)			= checkUsage (s,t) t2
checkUsage (s,t) (BoolNode t1 t2)				= checkUsage (s,t) t2
checkUsage (s,t) (OpNode _ t1 t2)				= checkUsage (s,t) t1 || checkUsage (s,t) t2
checkUsage (s,t) (BoolExNode (Comp _ t1 t2))	= checkUsage (s,t) t1 || checkUsage (s,t) t2
checkUsage (s,t) (BoolExNode (Boolean t1))		= checkUsage (s,t) t1
checkUsage (s,t) (GiftNode t1)					= checkUsage (s,t) t1
checkUsage (s,t) (PlunderNode t1)				= checkUsage (s,t) t1
checkUsage (s,t) (IfNode t1 xs)					| checkUsage (s,t) t1 || any (==True) (map (checkUsage (s,t)) xs) 	= True
												| otherwise 														= trace ("*** Warning: '" ++ s ++ "' is never used.") True
checkUsage (s,t) (ElseNode xs)					| any (==True) $ map (checkUsage (s,t)) xs 	= True
												| otherwise 								= trace ("*** Warning: '" ++ s ++ "' is never used.") True
checkUsage (s,t) (ForNode t1 t2 t3 xs)  		| checkUsage (s,t) t1 || checkUsage (s,t) t2 || all (==True) (map (checkUsage (s,t)) xs) 	= True
												| otherwise 																				= trace ("*** Warning: '" ++ s ++ "' is never used.") True
checkUsage (s,t) (WhileNode t1 xs)				| checkUsage (s,t) t1 || any (==True) (map (checkUsage (s,t)) xs) 	= True
												| otherwise 														= trace ("*** Warning: '" ++ s ++ "' is never used.") True
checkUsage (s,t) (FuncNode _ xs xs') 			| any (==True) (map (checkUsage (s,t)) xs') && all (==False) (map (checkUsage (s,t)) xs)	= True
												| otherwise 																				= trace ("*** Warning: '" ++ s ++ "' is never used.") True
checkUsage (s,t) (FuncValNode t1 t2)			= checkUsage (s,t) t1
checkUsage (s,t) (PrintNode t1)					= checkUsage (s,t) t1
checkUsage (s,t) (ReturnNode _ t1)				= checkUsage (s,t) t1
checkUsage (s,t) (DoFuncNode _ xs)				= any (==True) $ map (checkUsage (s,t)) xs
checkUsage (s,t) (ZupaNode _ xs)				| any (==True) $ map (checkUsage (s,t)) xs 	= True
												| otherwise 								= trace ("*** Warning: '" ++ s ++ "' is never used.") True

typeChecker :: [[(String, Types)]] -> Tree -> Bool
typeChecker list (OpNode "+" t1 t2)		= (checkType (OpNode "+" t1 t2) Int list) || (checkType (OpNode "+" t1 t2) Str list)
typeChecker list (OpNode s t1 t2)		= checkType (OpNode s t1 t2) Int list
typeChecker list (BoolExNode n)			= checkType (BoolExNode n) Boo list
typeChecker list (GiftNode t1)			= checkType t1 Int list
typeChecker list (PlunderNode t1)		= checkType t1 Int list
typeChecker list (IfNode t1 xs)	 		= all (==True) (map (typeChecker newList) allNodes) && checkType t1 Boo list
										where
								 			newList 	= (addToScope xs): list
								 			allNodes 	= getOtherNodes xs
typeChecker list (ElseNode xs)			= all (==True) (map (typeChecker newList) allNodes)
										where
											newList 	= (addToScope xs): list
											allNodes	= getOtherNodes xs
typeChecker list (ForNode t1 t2 t3 xs)	= all (==True) (map (typeChecker newList) allNodes) && typeChecker list t1 && typeChecker list t2 && typeChecker list t3
										where
								 			newList 	= (addToScope xs): list
								 			allNodes 	= getOtherNodes xs
typeChecker list (WhileNode t1 xs)		= all (==True) (map (typeChecker newList) allNodes) && checkType t1 Boo list
										where
											newList 	= (addToScope xs): list
											allNodes	= getOtherNodes xs
typeChecker list (FuncNode s xs xs')	= all (==True) (map (typeChecker list) xs) && all (==True) (map (typeChecker newList) allNodes)
										where
								 			newList 	= (addToScope (xs++xs')): list
								 			allNodes 	= getOtherNodes xs'
typeChecker list (FuncValNode t1 t2)	= (addToScope [FuncValNode t1 t2]) /= []
typeChecker list (ZupaNode s xs) 		= all (==True) (map (typeChecker newList) allNodes)
								 		where
								 			newList 	= (addToScope xs): list
								 			allNodes 	= getOtherNodes xs
typeChecker list _ = True

getOtherNodes :: [Tree] -> [Tree]
getOtherNodes []						= []
getOtherNodes ((BootyNode _ _): xs) 	= getOtherNodes xs
getOtherNodes ((DoubloonNode _ _): xs)	= getOtherNodes xs
getOtherNodes ((BoolNode _ _): xs)		= getOtherNodes xs
getOtherNodes ((TreasureNode _ _): xs)  = getOtherNodes xs
getOtherNodes (n:xs)					= n: getOtherNodes xs

addToScope :: [Tree] -> [(String, Types)]
addToScope [] 									= []
addToScope ((BootyNode (VarNode s l) t): xs)	| checkType t Str []	= (s, Str): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a String! Line:" ++ (show l))
addToScope ((DoubloonNode (VarNode s l) t): xs)	| checkType t Int []	= (s, Int): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a Integer! Line:" ++ (show l))
addToScope ((BoolNode (VarNode s l) t): xs)		| checkType t Boo []	= (s, Boo): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a Boolean! Line:" ++ (show l))
addToScope ((TreasureNode (VarNode s l) t): xs)	| checkType t Boo []	= (s, Arr): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a Boolean! Line:" ++ (show l))
addToScope ((FuncValNode (VarNode s l) (VarNode s2 l2)): xs)	
												= (s, (getTypeFromString s2)): addToScope xs 
addToScope (_: xs)								= addToScope xs

getTypeFromString :: String -> Types
getTypeFromString s
	| s == "String" = Str
	| s == "Int"	= Int
	| s == "Bool"	= Boo
	| s == "Array"	= Arr
	| otherwise		= Err

checkType :: Tree -> Types -> [[(String, Types)]] -> Bool
checkType (VarNode s l) Int list 					| all (==True) (map (isNumber) s) || getType s l list == Int 	= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an Integer! Line:" ++ (show l))
checkType (VarNode s l) Str list 					| isString s || getType s l list == Str						= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an String! Line:" ++ (show l))
checkType (VarNode s l) Boo list 					| s == "Aye" || s == "Nay" || getType s l list == Boo		= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an Boolean! Line:" ++ (show l))
checkType (OpNode s t1 t2) Int list					| checkType t1 Int list && checkType t2 Int list 			= True
													| otherwise													= error ("Incorrect Type: " ++ (getValue t1) ++ " and " ++ (getValue t2) ++ " are not Integers!")
checkType (OpNode "+" t1 t2) Str list 				| checkType t1 Str list && checkType t2 Str list 			= True
													| otherwise													= error ("Incorrect Type: " ++ (getValue t1) ++ " and " ++ (getValue t2) ++ " are not String!")
checkType (BoolExNode (Comp "be" t1 t2)) Boo list 	| checkType t1 t list && checkType t2 t list 				= True
													| otherwise													= error ("Incorrect Type: " ++ (getValue t1) ++ " and " ++ (getValue t2) ++ " are not Integers!")
													where
														t = getTreeType t1 list
checkType (BoolExNode (Comp _ t1 t2)) Boo list 		| checkType t1 Int list && checkType t1 Int list 			= True
													| otherwise													= error ("Incorrect Type: " ++ (getValue t1) ++ " and " ++ (getValue t2) ++ " are not Integers!")
checkType (BoolExNode (Boolean t1))	Boo	list		| checkType t1 Boo list										= True
													| otherwise													= error ("Incorrect Type: " ++ (getValue t1) ++ "is not an Boolean!")
checkType _ _ _										= False

getType :: String -> Int -> [[(String, Types)]] -> Types
getType	s l []						| isString s || all (==True) (map (isNumber) s) || isBoolean s = Err
									| otherwise										= error ("Declaration: '" ++ s ++ "' has not been defined yet! Line: " ++ (show l))
getType s l ([]:list)				= getType s l list
getType s l (((s2,t):tup):list) 	| s == s2 	= t
							  		| otherwise = getType s l (tup:list)

getTreeType :: Tree -> [[(String, Types)]] -> Types
getTreeType (VarNode s l) list 		= getType s l list
getTreeType (OpNode s t1 t2) list 	= getTreeType t1 list
getTreeType _ list 			 		= Err

isAccessible :: [[(String, Types)]] -> Tree -> Bool
isAccessible ([]:x:list) a 						= isAccessible (x:list) a 
isAccessible (((s2,_):tup):list) (VarNode s l)  | s == s2 || isString s || isNumber (head s) || isBoolean s	= True
												| otherwise = isAccessible (tup:list) (VarNode s l) 
isAccessible list (BootyNode t1 t2)				= isAccessible list t1 && isAccessible list t2
isAccessible list (DoubloonNode t1 t2) 			= isAccessible list t1 && isAccessible list t2
isAccessible list (BoolNode t1 t2) 				= isAccessible list t1 && isAccessible list t2
isAccessible list (OpNode _ t1 t2) 				= isAccessible list t1 && isAccessible list t2
isAccessible list (BoolExNode (Comp _ t1 t2))  	= isAccessible list t1 && isAccessible list t2
isAccessible list (BoolExNode (Boolean t1)) 	= isAccessible list t1
isAccessible list (GiftNode t1) 				= isAccessible list t1
isAccessible list (PlunderNode t1) 				= isAccessible list t1
isAccessible list (IfNode t1 xs)  				= isAccessible list t1 && all (==True) (map (isAccessible ((addToScope xs): list)) (getOtherNodes xs)) && (all (==True) (map (doUsage (IfNode t1 xs)) (addToScope xs)))
isAccessible list (ElseNode xs)					= all (==True) (map (isAccessible ((addToScope xs): list)) (getOtherNodes xs)) && (all (==True) (map (doUsage (ElseNode xs)) (addToScope xs)))
isAccessible list (ForNode t1 t2 t3 xs)			= isAccessible list t1 && isAccessible list t2 && isAccessible list t3 && all (==True) (map (isAccessible ((addToScope xs): list)) (getOtherNodes xs)) && (all (==True) (map (doUsage (ForNode t1 t2 t3 xs)) (addToScope xs)))
isAccessible list (WhileNode t1 xs)				= isAccessible list t1 && all (==True) (map (isAccessible ((addToScope xs): list)) (getOtherNodes xs)) && (all (==True) (map (doUsage (WhileNode t1 xs)) (addToScope xs)))
isAccessible list (FuncNode s xs xs')			= all (==True) (map (isAccessible list) xs) && all (==True) (map (isAccessible (((addToScope xs) ++ addToScope xs'): list)) xs') && (all (==True) (map (doUsage (FuncNode s xs xs')) ((addToScope xs) ++ addToScope xs')))
isAccessible list (FuncValNode t1 t2)			= isAccessible list t1 && isAccessible list t2
isAccessible list (PrintNode t1)				= isAccessible list t1
isAccessible list (ReturnNode s t1)				= isAccessible list t1
isAccessible list (DoFuncNode s xs)				= all (==True) (map (isAccessible list) xs)
isAccessible list (ZupaNode s xs)				= all (==True) (map (isAccessible newList) (getOtherNodes xs)) && (all (==True) (map (doUsage (ZupaNode s xs)) (addToScope xs)))
												where
													newList = (addToScope xs):list
isAccessible list _								= False

getValue :: Tree -> String
getValue (VarNode s _) = s
getValue (OpNode s _ _) = s
getValue (FuncNode s _ _) = s