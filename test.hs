import Data.Char
import Data.List
import Parse
import Grammar
import Checker
import Converter
import FPPrac.Trees

failSyntax1 = unlines ["Syntax { }"]
failSyntax2 = unlines [	"fleet Syntax {",
				   			"doubloon a be 1",
				  	    "}"
				  	  ]
failSyntax3 = unlines [	"fleet Syntax {",
				   			"a(doubloon i) {",
				   				"avast i, Arrr!",
				   			"}",
				  	    "}"
				  	  ]

succesSyntax1 = unlines ["fleet Syntax {",
							"doubloon a be 1, Arrr!",
							"flagship() { }",
						 "}"
						]
syntax1Tree   = 		ZupaNode "Syntax" 	[DoubloonNode (VarNode "a" 1 9) (VarNode "1" 1 14), 
											 FuncNode "flagship" [] []]

succesSyntax2 = unlines ["fleet Syntax {",
							"doubloonShip a(doubloon i) {",
								"avast i, Arrr!",
							"}",
							"flagship() { }",
						"}"
						]
syntax2Tree   =			ZupaNode "Syntax"	[IntFuncNode "a" [FuncValNode (VarNode "i" 1 24) (VarNode "Int" 0 0)] [ReturnNode "avast" (VarNode "i" 2 6)],
											 FuncNode "flagship" [] []]

succesSyntax3 = unlines ["fleet Syntax {",
						 	"doubloonShip a() {",
						 		"parley(Aye) {",
						 			"avast 1, Arrr!",
						 		"}",
						 	"}",
						 	"flagship() { }",
						 "}"
						]
syntax3Tree	  = 		ZupaNode "Syntax" [IntFuncNode "a" [] [IfNode (BoolExNode (Boolean (VarNode "Aye" 2 7))) [ReturnNode "avast" (VarNode "1" 3 6)]], 
										   FuncNode "flagship" [] []]

syntaxTest :: String -> Bool -> Bool
syntaxTest s b = ((parse grammar Program $ tokens s) /= (PLeaf ((Keyword "PH"), "PH", 0, 0))) == b

allSyntaxTests :: [Bool]
allSyntaxTests = [syntaxTest failSyntax1 False, syntaxTest failSyntax2 False, syntaxTest failSyntax3 False, syntaxTest succesSyntax1 True, syntaxTest succesSyntax1 True]

compareTree :: String -> Tree -> Bool
compareTree s tree = (convert (parse grammar Program (tokens s))) == tree

main = do putStrLn "SDSD"