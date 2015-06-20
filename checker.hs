import Data.Char
import FPPrac.Trees
import Data.List
import Debug.Trace
import Grammar

typeChecker :: [[(String, Types)]] -> Tree -> Bool
typeChecker list (IfNode t1 xs)	 	= all (==True) (map (typeChecker newList) allNodes) && checkType t1 Boo list
									where
								 		newList 	= (addToScope xs): list
								 		allNodes 	= getOtherNodes xs
typeChecker list (WhileNode t1 xs)	= all (==True) (map (typeChecker newList) allNodes) && checkType t1 Boo list
									where
										newList 	= (addToScope xs): list
										allNodes	= getOtherNodes xs
typeChecker list (ZupaNode s xs) 	= all (==True) (map (typeChecker newList) allNodes)
								 	where
								 		newList 	= (addToScope xs): list
								 		allNodes 	= getOtherNodes xs
typeChecker list _ = True

getOtherNodes :: [Tree] -> [Tree]
getOtherNodes []						= []
getOtherNodes ((BootyNode _ _): xs) 	= getOtherNodes xs
getOtherNodes ((DoubloonNode _ _): xs)	= getOtherNodes xs
getOtherNodes ((BoolNode _ _): xs)		= getOtherNodes xs
getOtherNodes (n:xs)					= n: getOtherNodes xs

as (ZupaNode s xs) = addToScope xs

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
addToScope (_: xs)								= addToScope xs

checkType :: Tree -> Types -> [[(String, Types)]] -> Bool
checkType (VarNode s l) Int list 					| all (==True) (map (isNumber) s) || getType s l list == Int 	= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an Integer! Line:" ++ (show l))
checkType (VarNode s l) Str list 					| isString s || getType s l list == Str						= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an String! Line:" ++ (show l))
checkType (VarNode s l) Boo list 					| s == "Aye" || s == "Nay" || getType s l list == Boo			= True
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
getType	s l []						= Err
getType s l ([]:list)				= getType s l list
getType s l (((s2,t):tup):list) 	| s == s2 	= t
							  		| otherwise = getType s l (tup:list)

getTreeType :: Tree -> [[(String, Types)]] -> Types
getTreeType (VarNode s l) list 		= getType s l list
getTreeType (OpNode s t1 t2) list 	= getTreeType t1 list
getTreeType _ list 			 		= Err

isAccesible :: [[(String, Types)]] -> Tree -> Bool
isAccesible [] a								= False
isAccesible ([]:x:list) a 						= isAccesible (x:list) a 
isAccesible (((s2,_):tup):list) (VarNode s l)  	| s == s2 	= True
												| otherwise = isAccesible (tup:list) (VarNode s l) 
isAccesible list (BootyNode t1 t2)				= isAccesible list t1 && isAccesible list t2
isAccesible list (DoubloonNode t1 t2) 			= isAccesible list t1 && isAccesible list t2
isAccesible list (BoolNode t1 t2) 				= isAccesible list t1 && isAccesible list t2
isAccesible list (OpNode _ t1 t2) 				= isAccesible list t1 && isAccesible list t2
isAccesible list (BoolExNode (Comp _ t1 t2))  	= isAccesible list t1 && isAccesible list t2
isAccesible list (BoolExNode (Boolean t1)) 		= isAccesible list t1
isAccesible list (GiftNode t1) 					= isAccesible list t1
isAccesible list (PlunderNode t1) 				= isAccesible list t1
isAccesible list (IfNode t1 xs)  				= isAccesible list t1 && all (==True) (map (isAccesible list) xs) 
isAccesible list (ElseNode xs)					= all (==True) (map (isAccesible list) xs)
isAccesible list (ForNode t1 t2 t3 xs)			= isAccesible list t1 && isAccesible list t2 && isAccesible list t3 && all (==True) (map (isAccesible list) xs)
isAccesible list (WhileNode t1 xs)				= isAccesible list t1 && all (==True) (map (isAccesible list) xs)
isAccesible list (FuncNode s xs xs')			= all (==True) (map (isAccesible list) xs) && all (==True) (map (isAccesible list) xs')
isAccesible list (FuncValNode t1 t2)			= isAccesible list t1 && isAccesible list t2
isAccesible list (PrintNode t1)					= isAccesible list t1
isAccesible list (ReturnNode s t1)				= isAccesible list t1
isAccesible list (DoFuncNode s xs)				= all (==True) (map (isAccesible list) xs)
isAccesible list (ZupaNode s xs)				= all (==True) (map (isAccesible list) xs)
isAccesible	list _								= False

getValue :: Tree -> String
getValue (VarNode s _) = s
getValue (OpNode s _ _) = s
getValue (FuncNode s _ _) = s