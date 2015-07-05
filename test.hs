import Data.Char
import Data.List
import Parse
import Grammar
import Checker
import Converter
import FPPrac.Trees
import Control.Exception
import Control.Monad
import Debug.Trace

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
syntax3Tree	  = 		ZupaNode "Syntax" [IntFuncNode "a" []
 												[IfNode (BoolExNode (Boolean (VarNode "Aye" 2 7))) 
 													[ReturnNode "avast" (VarNode "1" 3 6)]
 												], 
										   FuncNode "flagship" [] []]

succesSyntax4 = unlines ["fleet Syntax {",	
							"doubloonShip a(doubloon i) {",
								"parley(i be 5) {",
									"avast i, Arrr!",
								"}",
								"heave {",
									"avast i, Arrr!",
								"}",
							"}",
							"flagship() { }",
						"}"]
syntax4Tree   = 		ZupaNode "Syntax" [IntFuncNode "a" [FuncValNode (VarNode "i" 1 24) (VarNode "Int" 0 0)] 
												[IfElseNode (BoolExNode (Comp "be" (VarNode "i" 2 7) (VarNode "5" 2 11))) 
													[ReturnNode "avast" (VarNode "i" 3 6)] 
												(ElseNode 
													[ReturnNode "avast" (VarNode "i" 6 6)]
												)],
										   FuncNode "flagship" [] []]

succesSyntax5 = unlines ["fleet Syntax {",
						 	"doubloonShip a() {",
						 		"navigate(doubloon i be 0. i be below 5. gift i) {",
						 			"parrot i, Arrr!",
						 		"}",
						 	"}",
						 	"flagship() { }",
						 "}"]
syntax5Tree   = 		ZupaNode "Syntax" [IntFuncNode "a" [] [ForNode (DoubloonNode (VarNode "i" 2 18) (VarNode "0" 2 22)) (BoolExNode (Comp "be below" (VarNode "i" 2 25) (VarNode "5" 2 36))) (GiftNode (VarNode "i" 2 44)) 
																	[PrintNode (VarNode "i" 3 7)]
															  ],
										   FuncNode "flagship" [] []] 

succesSyntax6 = unlines ["fleet Syntax {",
							"doubloonShip a(order o) {",
								"whirlpool(o) {",
									"parrot o, Arrr!",
								"}",
							"}",
							"flagship() { }",
						"}"]
syntax6Tree   = 		ZupaNode "Syntax" [IntFuncNode "a" [FuncValNode (VarNode "o" 1 21) (VarNode "Bool" 0 0)] [WhileNode (BoolExNode (Boolean (VarNode "o" 2 10))) 
																	[PrintNode (VarNode "o" 3 7)]
															  ],
															  FuncNode "flagship" [] []]

syntaxTest :: String -> Bool -> IO Bool
syntaxTest s b = do
    result <- try (return (parse grammar Program $ tokens s)) :: IO (Either SomeException ParseTree)
    case result of
        Left ex  -> return False
        Right val -> return ((val /= (PLeaf ((Keyword "PH"), "PH", 0, 0))) == b)


allSyntaxTests :: [IO Bool]
allSyntaxTests = [syntaxTest failSyntax1 False, syntaxTest failSyntax2 False, syntaxTest failSyntax3 False, 
				  syntaxTest succesSyntax1 True, syntaxTest succesSyntax2 True, syntaxTest succesSyntax3 True, syntaxTest succesSyntax4 True, syntaxTest succesSyntax5 True, syntaxTest succesSyntax6 True]

compareTree :: String -> Tree -> Bool
compareTree s tree = (convert (parse grammar Program (tokens s))) == tree
