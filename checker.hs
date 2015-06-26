module Checker where

import Data.Char
import FPPrac.Trees
import Data.List
import Debug.Trace
import Grammar

--main = do putStr (show (typeAndScopeChecker $ convert test0))

typeAndScopeChecker :: Tree -> Bool
typeAndScopeChecker node = typeChecker [] node && inScope [] node

doUsage :: Tree -> (String, Types) -> Bool
doUsage node a = checkUsage a node

checkUsage :: (String, Types) -> Tree -> Bool
checkUsage t@(s,p) tree = 
	case tree of
		(VarNode s2 l c)			-> s == s2
		(BootyNode t1 t2) 			-> usage t t2
		(DoubloonNode t1 t2) 		-> usage t t2
		(BoolNode t1 t2)			-> usage t t2
		(AssignNode t1 t2) 			-> usage t t1 || usage t t2
		(OpNode _ t1 t2)			-> usage t t1 || usage t t2
		(BoolExNode (Comp _ t1 t2))	-> usage t t1 || usage t t2
		(BoolExNode (Boolean t1))	-> usage t t1
		(GiftNode t1)				-> usage t t1
		(ArrayNode t1 t2 _)			-> usage t t2
		(PlunderNode t1)			-> usage t t1
		(FuncValNode t1 t2)			-> usage t t1
		(PrintNode t1)				-> usage t t1
		(ReturnNode _ t1)			-> usage t t1
		(DoFuncNode _ xs)			-> uMap t xs
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
		(ZupaNode _ xs)				| uMap t xs								-> True
									| otherwise 							-> warning s
warning s	= trace ("*** Warning: '" ++ s ++ "' is never used.") True
usage a b	= checkUsage a b
uMap a b	= anyT (map (checkUsage a) b)
falseMap a b= all (==False) (map (checkUsage a) b)

typeChecker :: [[(String, Types)]] -> Tree -> Bool
typeChecker list tree =
	case tree of
		n@(OpNode "+" t1 t2)	-> checkType n  Int list || checkType n Str list
		n@(OpNode s t1 t2)		-> checkType n  Int list
		n@(BoolExNode t1)		-> checkType n  Boo list
		(GiftNode t1)			-> checkType t1 Int list
		(PlunderNode t1)		-> checkType t1 Int list
		(AssignNode t1 t2) 		-> checkAssignType t1 t2 list
		(ElseNode xs)			-> tCheckerMap xs list
		(ZupaNode s xs) 		-> tCheckerMap xs list 
		(FuncNode s xs xs')		-> tCheckerMap xs list && tMap list xs
		(IfNode t1 xs)			-> tCheckerMap xs list && checkType t1 Boo list
		(IfElseNode t1 xs xs')	-> tCheckerMap xs list && typeChecker list xs' && checkType t1 Boo list
		(WhileNode t1 xs)		-> tCheckerMap xs list && checkType t1 Boo list
		(ForNode t1 t2 t3 xs)	-> tCheckerMap xs list && typeChecker list t1 && 
								   typeChecker list t2 && typeChecker list t3
		n@(FuncValNode t1 t2)	-> addToScope [n] /= []
		_ 						-> True
tMap a b 		= allT (map (typeChecker a) b)
tCheckerMap a b = tMap ((addToScope a): b) (getOtherNodes a)

getOtherNodes :: [Tree] -> [Tree]
getOtherNodes []						= []
getOtherNodes ((BootyNode _ _): xs) 	= getOtherNodes xs
getOtherNodes ((DoubloonNode _ _): xs)	= getOtherNodes xs
getOtherNodes ((BoolNode _ _): xs)		= getOtherNodes xs
getOtherNodes ((TreasureNode _ _): xs)  = getOtherNodes xs
getOtherNodes (n:xs)					= n: getOtherNodes xs

addToScope :: [Tree] -> [(String, Types)]
addToScope [] 										= []
addToScope ((BootyNode (VarNode s l c) t): xs)		| checkType t Str []	= (s, Str): addToScope xs 
													| otherwise 			= incType t "String" l c 
addToScope ((DoubloonNode (VarNode s l c) t): xs)	| checkType t Int []	= (s, Int): addToScope xs 
													| otherwise 			= incType t "Integer" l c
addToScope ((BoolNode (VarNode s l c) t): xs)		| checkType t Boo []	= (s, Boo): addToScope xs 
													| otherwise 			= incType t "Boolean" l c
addToScope ((ArrayNode (VarNode s l c) t v): xs)	| checkType t Boo []	= (s, Arr): addToScope xs 
													| otherwise 			= incType t "Array" l c
addToScope ((FuncValNode (VarNode s l c) (VarNode s2 l2 c2)): xs)	
													= (s, (getTypeFromString s2)): addToScope xs 
addToScope (_: xs)									= addToScope xs

incType a s l c = error ("Incorrect Type: " ++ (getValue a) ++ " is not a " ++ s ++ " ! Line:" ++ (show l) ++ ":" ++ (show c))

checkAssignType :: Tree -> Tree -> [[(String, Types)]] -> Bool
checkAssignType idf val list = checkType val (getTreeType idf list) list

getTypeFromString :: String -> Types
getTypeFromString s
	| s == "String" = Str
	| s == "Int"	= Int
	| s == "Bool"	= Boo
	| s == "Array"	= Arr
	| otherwise		= Err

getStringFromType :: Types -> String
getStringFromType s
	| s == Str 		= "String" 
	| s == Int 		= "Int"
	| s == Boo 		= "Bool"
	| s == Arr 		= "Array"
	| otherwise		= "Error"

checkType :: Tree -> Types -> [[(String, Types)]] -> Bool
checkType tree Int list =
	case tree of 
		n@(VarNode s l c)  					| allT (map (isNumber) s) || getType s l c list == Int 	-> True
											| otherwise												-> wrongType n "Integer" l c 
		(OpNode s t1 t2)					| checkType t1 Int list && checkType t2 Int list 		-> True
											| otherwise												-> False
		_									-> False

checkType tree Str list =
	case tree of
		n@(VarNode s l c) 					| isString s || getType s l c list == Str					-> True
											| otherwise												-> wrongType n "String" l c
		(OpNode "+" t1 t2)					| checkType t1 Str list && checkType t2 Str list 		-> True
											| otherwise												-> False
		_									-> False

checkType tree Boo list =
	case tree of
		n@(VarNode s l c)	 				| s == "Aye" || s == "Nay" || getType s l c list == Boo	-> True
											| otherwise												-> wrongType n "Boolean" l c
		(BoolExNode (Comp "be" t1 t2))  	| checkType t1 t list && checkType t2 t list 			-> True
											| otherwise												-> False
											where
												t = getTreeType t1 list
		(BoolExNode (Boolean t1))			| checkType t1 Boo list									-> True
											| otherwise												-> wrongType t1 "Boolean" (getL t1) (getC t1)
		(BoolExNode (Comp _ t1 t2)) 		| checkType t1 Int list && checkType t1 Int list 		-> True
											| otherwise												-> False
		_									-> False
wrongType a b c d	= error ("Incorrect Type: " ++ (getValue a) ++ " is not an " ++ b ++ "! Line: " ++ (show c) ++ ":" ++ (show d))
getL (VarNode _ l _) = l
getL _ 				 = 0 
getC (VarNode _ _ c) = c
getC _ 				 = 0 

getType :: String -> Int -> Int -> [[(String, Types)]] -> Types
getType	s l c []					| isString s || allT (map (isNumber) s) || isBoolean s 	= Err
									| otherwise												= error ("Declaration: '" ++ s ++ "' has not been defined yet! Line: " ++ (show l) ++ ":" ++ (show c))
getType s l c ([]:list)				= getType s l c list
getType s l c (((s2,t):tup):list) 	| s == s2 	= t
							  		| otherwise = getType s l c (tup:list)

getTreeType :: Tree -> [[(String, Types)]] -> Types
getTreeType (VarNode s l c) list 		= getType s l c list
getTreeType (OpNode s t1 t2) list 		= getTreeType t1 list
getTreeType _ list 			 			= Err

inScope :: [[(String, Types)]] -> Tree -> Bool
inScope ([]:x:list) a 							= inScope (x:list) a 
inScope (((s2,_):tup):list) n@(VarNode s l c) 	| s == s2 || isString s || isNumber (head s) || isBoolean s	= True
												| otherwise = inScope (tup:list) n
inScope list tree =
	case tree of
		(BoolExNode (Boolean t1)) 	-> inScope list t1
		(ArrayNode t1 t2 _)			-> inScope list t2
		(GiftNode t1) 				-> inScope list t1
		(PlunderNode t1) 			-> inScope list t1
		(PrintNode t1)				-> inScope list t1
		(ReturnNode s t1)			-> inScope list t1
		(BootyNode t1 t2)			-> inScope list t1 && inScope list t2
		(DoubloonNode t1 t2) 		-> inScope list t1 && inScope list t2
		(BoolNode t1 t2) 			-> inScope list t1 && inScope list t2
		(AssignNode t1 t2)			-> inScope list t1 && inScope list t2
		(OpNode _ t1 t2) 			-> inScope list t1 && inScope list t2
		(BoolExNode (Comp _ t1 t2)) -> inScope list t1 && inScope list t2
		(FuncValNode t1 t2)			-> inScope list t1 && inScope list t2
		n@(ForNode t1 t2 t3 xs)		-> inScope list t1 && inScope list t2 && inScope list t3 && scopeM xs list
		n@(IfNode t1 xs)			-> inScope list t1 && scopeM xs list
		n@(IfElseNode t1 xs xs')	-> inScope list t1 && scopeM xs list && inScope list xs'
		n@(WhileNode t1 xs)			-> inScope list t1 && scopeM xs list 
		n@(FuncNode s xs xs')		-> scopeM2 list xs && funcM xs xs' list
		(DoFuncNode s xs)			-> scopeM2 list xs
		n@(ElseNode xs)				-> scopeM xs list 
		n@(ZupaNode s xs)			-> scopeM xs list  && usageM n n
		_							-> False
usageM a b	= allT (map (doUsage a) (addAllToScope b))
scopeM a b 	= scopeM2 ((addToScope a): b) (getOtherNodes a)
funcM a b c = scopeM2 (((addToScope a) ++ (addToScope b)): c) b
scopeM2 a b = allT (map (inScope a) b)
 
addAllToScope :: Tree -> [(String, Types)]
addAllToScope tree =
	case tree of 
		(IfNode _ xs)		 -> (addToScope xs) ++ (concat (map addAllToScope xs))
		(IfElseNode _ xs xs')-> (addToScope xs) ++ (concat (map addAllToScope xs) ++ addAllToScope xs')
		(ElseNode xs)		 -> (addToScope xs) ++ (concat (map addAllToScope xs))
		(WhileNode _ xs)	 -> (addToScope xs) ++ (concat (map addAllToScope xs))
		(ZupaNode _ xs)		 -> (addToScope xs) ++ (concat (map addAllToScope xs))
		(ForNode xs _ _ xs') -> (addToScope [xs]) ++ (addToScope xs') ++ (concat (map addAllToScope xs'))
		(FuncNode _ xs xs')  -> (addToScope xs) ++ (addToScope xs') ++ (concat (map addAllToScope xs'))
		_					 -> []

getValue :: Tree -> String
getValue (VarNode s _ _) 	| s == "Aye" = "1"
					   	| s == "Nay" = "0"
						| otherwise	= s
getValue (OpNode s _ _) = s
getValue (FuncNode s _ _) = s
getValue (DoubloonNode t1 t2) = "doubloon " ++ getValue t1 ++ " be " ++ getValue t2
getValue (FuncValNode t1 t2) = getValue t1 ++ " " ++ getValue t2
getValue (GiftNode t1) = "gift " ++ getValue t1

allT = all (==True)
anyT = any (==True)