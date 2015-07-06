module Context where

import Parse
import Grammar
import Checker
import Converter
import FPPrac.Trees
import Control.Exception
import Control.Monad

{-
	Wrong --> Doubloon 'a' is re-declared in the same scope.
-}
context1 = ZupaNode "Context" [DoubloonNode (VarNode "a" 1 9) (VarNode "1" 1 14), 
							   DoubloonNode (VarNode "a" 2 9) (VarNode "3" 2 14), 
							   FuncNode "flagship" [] []]

{-
	Wrong --> Variable 'c' is not yet declared.
-}
context2 = ZupaNode "Context" [DoubloonNode (VarNode "a" 1 9) (VarNode "1" 1 14),
							   DoubloonNode (VarNode "b" 2 9) (OpNode "+"
							   										(VarNode "c" 2 14) 
							   										(VarNode "1" 2 16)),
							   FuncNode "flagship" [] []]

{-
	Right --> Doubloon 'a' is properly declared with an Integer
-}
context3 = ZupaNode "Context" [DoubloonNode (VarNode "a" 1 9) (VarNode "1" 1 14),
							   FuncNode "flagship" [] []]

{-
	Right --> Order 'i' is properly declared with an Boolean and is properly used in the If-Statement
-}
context4 = ZupaNode "Context" [IntFuncNode "a" [] [
									BoolNode (VarNode "i" 2 9) (VarNode "Aye" 2 19),
									IfNode (BoolExNode (Boolean (VarNode "i" 3 13))) []],
							   FuncNode "flagship" [] []]

{-
	Wrong --> Doubloon is initiated with a Boolean instead of an Integer
-}
context5 = ZupaNode "Context" [DoubloonNode (VarNode "a" 1 9) (VarNode "Aye" 1 14),
							   FuncNode "flagship" [] []]

{-
	Wrong --> The If-Statement tries to check the Doubloon 'i' which is not a Boolean
-}
context6 = ZupaNode "Context" [IntFuncNode "a" [] [
									DoubloonNode (VarNode "i" 2 9) (VarNode "1" 2 19),
									IfNode (BoolExNode (Boolean (VarNode "i" 3 13))) []],
							   FuncNode "flagship" [] []]

{- 
	Right --> Doubloon 'j' is being initiated by the doubloon 'i' declared in a former scope.
-}
context7 = ZupaNode "Context" [DoubloonNode (VarNode "i" 2 9) (VarNode "1" 2 19),
							   IntFuncNode "a" [] [
							   		DoubloonNode (VarNode "j" 4 9) (VarNode "i" 4 19)],
							   FuncNode "flagship" [] []]


doContextTest t = typeAndScopeChecker t
