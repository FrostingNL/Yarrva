import Data.Char
import FPPrac.Trees
import Data.List
import Grammar

typeChecker :: [[(String, Types)]] -> Tree -> Bool
typeChecker list (ZupaNode s xs) = all (==True) (map (typeChecker newList) allNodes)
								 where
								 	newList 	= (addToScope xs): list
								 	allNodes 	= getOtherNodes xs

getOtherNodes :: [Tree] -> [Tree]
getOtherNodes []						= []
getOtherNodes ((BootyNode _ _): xs) 	= getOtherNodes xs
getOtherNodes ((DoubloonNode _ _): xs)	= getOtherNodes xs
getOtherNodes ((BoolNode _ _): xs)		= getOtherNodes xs
getOtherNodes (n:xs)					= n: getOtherNodes xs

addToScope :: [Tree] -> [(String, Types)]
addToScope [] 									= []
addToScope ((BootyNode (VarNode s) t): xs)		| checkType t Str 	= (s, Str): addToScope xs 
												| otherwise 		= (s, Err): addToScope xs
addToScope ((DoubloonNode (VarNode s) t): xs)	| checkType t Int 	= (s, Int): addToScope xs 
												| otherwise 		= (s, Err): addToScope xs
addToScope ((BoolNode (VarNode s) t): xs)		| checkType t Boo 	= (s, Boo): addToScope xs 
												| otherwise 		= (s, Err): addToScope xs
addToScope (_: xs)								= addToScope xs

checkType :: Tree -> Types -> Bool
checkType (VarNode s) Int 				= all (==True) (map (isNumber) s)
checkType (VarNode s) Str 				= all (==True) (map (isNumber) s)
checkType (VarNode s) Boo 				= all (==True) (map (isNumber) s)
checkType (OpNode s t1 t2) Int			= checkType t1 Int && checkType t2 Int
checkType (OpNode "+" t1 t2) Str 		= checkType t1 Str && checkType t2 Str
checkType _ _ 							= False

getType :: String -> [[(String, Types)]] -> Types
getType	s []					= Err
getType s ([]:x:list)			= getType s (x:list)
getType s (((s2,t):tup):list) 	| s == s2 	= t
							  	| otherwise = getType s (tup:list)

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