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
addToScope ((BootyNode (VarNode s) t): xs)		| checkType t Str []	= (s, Str): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a String!")
addToScope ((DoubloonNode (VarNode s) t): xs)	| checkType t Int []	= (s, Int): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a Integer!")
addToScope ((BoolNode (VarNode s) t): xs)		| checkType t Boo []	= (s, Boo): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a Boolean!")
addToScope ((TreasureNode (VarNode s) t): xs)	| checkType t Boo []	= (s, Arr): addToScope xs 
												| otherwise 			= error ("Incorrect Type: " ++ (getValue t) ++ " is not a Boolean!")
addToScope (_: xs)								= addToScope xs

checkType :: Tree -> Types -> [[(String, Types)]] -> Bool
checkType (VarNode s) Int list 						| all (==True) (map (isNumber) s) || getType s list == Int 	= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an Integer!")
checkType (VarNode s) Str list 						| isString s || getType s list == Str						= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an String!")
checkType (VarNode s) Boo list 						| s == "Aye" || s == "Nay" || getType s list == Boo			= True
													| otherwise													= error ("Incorrect Type: " ++ s ++ " is not an Boolean!")
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

getType :: String -> [[(String, Types)]] -> Types
getType	s []					= error ("Declaration: " ++ s ++ " isn't declared yet!")
getType s ([]:list)				= getType s (list)
getType s (((s2,t):tup):list) 	| s == s2 	= t
							  	| otherwise = getType s (tup:list)

getTreeType :: Tree -> [[(String, Types)]] -> Types
getTreeType (VarNode s) list 		= getType s list
getTreeType (OpNode s t1 t2) list 	= getTreeType t1 list
getTreeType _ list 			 		= Err

isAccesible :: [[(String, Types)]] -> Tree -> Bool
isAccesible [] a								= False
isAccesible ([]:x:list) a 						= isAccesible (x:list) a 
isAccesible (((s2,_):tup):list) (VarNode s)  	| s == s2 	= True
												| otherwise = isAccesible (tup:list) (VarNode s) 
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
getValue (VarNode s) = s
getValue (OpNode s _ _) = s
getValue (FuncNode s _ _) = s