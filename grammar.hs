import Parse
import Data.Char
import Data.List
import Debug.Trace
import FPPrac.Trees
import System.IO

grammar :: Grammar
grammar nt = case nt of 																			-- The Grammar sorted by occurence
	Program -> [[progKey, idf, Block]]																			-- The Main Program
	Stat 	-> [[Opt [varKey], idf, NoCat equalsKey, Expr, NoCat endmark],										-- Var declaration
				[ifExprKey, lpar, BoolExpr, rpar, Block],														-- If Expression
				[elseKey, Block],																				-- Else Expression
				[printKey, Expr, NoCat endmark],																-- Print Expression
				[incKey, Type, NoCat endmark],
				[decKey, Type, NoCat endmark],
				[forKey, lpar, Assign, point, BoolExpr, point, Expr, rpar, Block],								-- For Expression
				[whileKey, lpar, BoolExpr, rpar, Block],														-- While Expression
				[Func, endmark],
				[returnKey, Opt [Expr], NoCat endmark],															-- Return Expression
				[functionKey, idf, lpar, FValues, rpar, Block],													-- Normal Function
				[mainKey, lpar, rpar, Block]]																	-- Main Function
	Block	-> [[lcbr, Rep0 [Stat], rcbr]]																		-- A block of code
	BoolExpr-> [[Expr, Alt [equalsKey] [Alt [lesserKey] [greaterKey]], Expr],									-- A boolean expression
				[Bool],																							-- A boolean
				[idf],																							-- An identifier
				[BoolExpr, Alt [orKey] [andKey], BoolExpr]] 													-- Two boolean expressions
	Expr 	-> [[Opt [lpar], Expr2, Opt [rpar]]]
	Expr2 	-> [[Type, SyntCat Op, Type],																		-- An expression
				[incKey, Type],																					-- Increase Type by 1
				[decKey, Type],																					-- Decrease Type by 1
				[Type]]																							-- One of the types
	Op		-> [[plus],																							-- Self Explanatory
				[minus],																						-- Self Explanatory
				[times],																						-- Self Explanatory
				[divide],																						-- Self Explanatory
				[notSym]]																						-- Self Explanatory
	Type	-> [[SyntCat Nmbr],																					-- A number
				[SyntCat Bool],																					-- A boolean
				[idf],																							-- An identifier
				[Func]]
	TypeName-> [[Keyword "Doubloon"],																			-- The name of the Type Number
				[Keyword "Bool"]]																				-- The name of the Type Boolean
	FValues -> [[Opt [FuncVal, Rep0 [NoCat comma, FuncVal]]]]
	FuncVal	-> [[varKey, idf, equalsKey, TypeName]]																-- The variable you can use in a function decleration
	Assign 	-> [[Opt [varKey], idf, NoCat equalsKey, Expr]]														-- Assign decleration
	Func 	-> [[idf, lpar, Opt [Expr, Rep0 [NoCat comma, Expr]], rpar]]																	-- Function

progKey 	= Keyword "fleet"
functionKey = Keyword "ship"
mainKey		= Keyword "flagship"
returnKey 	= Keyword "avast"
equalsKey 	= Keyword "be"	
lesserKey	= Keyword "below"
greaterKey	= Keyword "above"
trueKey 	= Keyword "Aye"
falseKey 	= Keyword "Nay"
varKey 		= Keyword "booty"
ifExprKey	= Keyword "parley"
elseKey 	= Keyword "heave"
breakKey	= Keyword "belay"
printKey	= Keyword "parrot"
continueKey = Keyword "God's speed"
whileKey	= Keyword "whirlpool"
forKey		= Keyword "navigate"
orKey		= Keyword "or"
andKey		= Keyword "'n"
incKey		= Keyword "gift"
decKey		= Keyword "plunder"
endmark		= Keyword ", Arrr!"

lpar    = Symbol "("
rpar    = Symbol ")"
lbra    = Symbol "["
rbra    = Symbol "]"
lcbr    = Symbol "{"
rcbr    = Symbol "}"
eq	    = Symbol "="
lt	    = Symbol "<"
ge      = Symbol ">"
plus    = Symbol "+"
minus   = Symbol "-"
times   = Symbol "*"
divide  = Symbol "/"
notSym  = Symbol "~"
colon   = Symbol ":"
point	= Symbol "."
comma	= Symbol ","

data State = START | ERROR | KW | KWWORD

tokenizer :: State -> String -> [Token]
tokenizer _ [] = []
tokenizer s (' ':xs) = tokenizer s xs
tokenizer s ('\t':xs) = tokenizer s xs
tokenizer s ('\n':xs) = tokenizer s xs

tokenizer ERROR _ = error "Shiver me timbers! You done it wrong, Arrr!"

tokenizer START (x:xs) | ord x >= 97 && ord x <= 122 = tokenizer KW (x:xs)
					   | otherwise = tokenizer ERROR (x:xs)

tokenizer KW ('{':xs) = (lcbr, ['{']): tokenizer KW xs
tokenizer KW ('}':xs) = (rcbr, ['}']): tokenizer KW xs
tokenizer KW ('(':xs) = (lpar, ['(']): tokenizer KW xs
tokenizer KW (')':xs) = (rpar, [')']): tokenizer KW xs
tokenizer KW ('.':xs) = (point, ['.']): tokenizer KW xs
tokenizer KW (',':xs) = (comma, [',']): tokenizer KW xs
tokenizer KW (x:xs) 
	| startsWith getEndmark (x:xs)					= (endmark, getEndmark): 				tokenizer KW (rmEndMark (x:xs))
	| isBoolean (x:restWord)						= (Bool, x:restWord) : 					otherTokens
	| isKeyword (x:restWord) 						= (Keyword (x:restWord), x:restWord): 	otherTokens
	| elem x "+-*/"									= (Op, [x]):							tokenizer KW (restWord ++ restString) 
	| isNumber x									= (Nmbr, x:restNumber): 				otherTokens
	| otherwise										= (Idf, (x:restWord)): 					otherTokens
	where
		restNumber 	= getNum xs
		restWord 	= getWord xs
		restString 	= getRest xs
		otherTokens = tokenizer KW restString

rmEndMark :: String -> String
rmEndMark [] = []
rmEndMark (',':' ':'A':'r':'r':'r':'!':xs) = xs

isKeyword :: String -> Bool
isKeyword s = elem s allKeywords

isBoolean :: String -> Bool
isBoolean word 
	| word == "Aye" || word == "Nay" = True
	| otherwise = False

getNum :: String -> String
getNum [] = []
getNum (x:xs)
	| not (isNumber x) = []
	| otherwise = x : getNum xs

getWord :: String -> String
getWord [] = []
getWord (x:xs)
	| elem x " +-*/,()." = []
	| otherwise = x: getWord xs

getRest :: String -> String
getRest [] = []
getRest (x:xs)
	| elem x ",+-*/()." = (x:xs)
	| x == ' ' = xs
	| otherwise = getRest xs

getEndmark :: String
getEndmark = ", Arrr!"

allKeywords :: [String]
allKeywords = ["fleet", "flagship", "ship", "avast", "be", "below", "above", "Aye", "Nay", "booty", "parley", "heave", "belay", "parrot", "God's speed", "whirlpool", "navigate"]

startsWith :: String -> String -> Bool
startsWith [] _ 	= True
startsWith _ [] 	= False
startsWith (s:search) (w:word) 
	| s == w = startsWith search word
	| otherwise = False

helloWorld = concat ["fleet HelloWorld {",
					 "   parrot \"Ahoy World!\", Arrr!",
					 "}"
					]

sampleFunction = concat ["fleet SampleFunction {",
						 "   ship add3(booty i be Doubloon, booty a be Bool) {",
						 "      avast i + 3, Arrr!",
						 "   }",
						 "   ",
						 "   flagship() {",
						 "      booty a be add3(5), Arrr!",
						 "      parrot a, Arrr!",
						 "   }",
						 "}"
						]

test = concat ["fleet Prog {",
			   "    booty a be 1, Arrr!",
			   "    booty b be 2+3, Arrr!",
			   "    parley (b be a) {",
			   "        booty c be 1, Arrr!",
			   "        navigate (booty i be 0. i below 5. gift i) {",
			   "            booty c be c+1, Arrr!",
			   "            booty d be Aye, Arrr!",
			   "        }",
			   "    }",
			   "}"
			   ]

test2 = concat ["fleet Fib {",
				"    countDown(a,b), Arrr!",
				"}"
				]

tokens = tokenizer START

test0 = parse grammar Program $ tokens test
test1 = parse grammar Program $ tokens test2

showTestTree = showRoseTree $ toRoseTree1 test0
showTestTree2 = showRoseTree $ toRoseTree1 test1

printTupList :: [(Alphabet, String)] -> IO String
printTupList [t] = do return (show t)
printTupList (t:tup) = do
						putStrLn (show t)
						printTupList tup

file :: FilePath -> IO ()
file f = do  
	handle <- openFile f ReadMode  
	contents <- hGetContents handle
	showRoseTreeList [toRoseTree1 $ parse grammar Program $ tokens contents, toRTree $ convert $ parse grammar Program $ tokens contents]

convert :: ParseTree -> Tree
convert (PLeaf (a, s)) = VarNode s
convert (PNode _ ((PLeaf (Keyword "booty", "booty")): x: x':[]))						= BootyNode (convert x) (convert x')
convert (PNode _ (x: (PLeaf (Op,s)): x': []))											= OpNode 	s (convert x) (convert x')
convert (PNode _ [PLeaf (a, s)])														| s == "Doubloon" 	= VarNode "Int"
																						| otherwise 		= VarNode s
convert (PNode _ [node])																= convert node
convert (PNode _ ((PLeaf (Keyword "parley", s)): x: (PNode Block xs): []))				= IfNode 	(convert x) (map convert xs)
convert (PNode _ ((PLeaf (Keyword "heave", s)): (PNode Block xs): []))					= ElseNode  (map convert xs)
convert (PNode _ (x: (PLeaf (Keyword "be",s)): x': []))									= BoolExNode $ Comp s (convert x) (convert x')
convert (PNode _ (x: (PLeaf (Keyword "below",s)): x': []))								= BoolExNode $ Comp s (convert x) (convert x')
convert (PNode _ (x: (PLeaf (Keyword "above",s)): x': []))								= BoolExNode $ Comp s (convert x) (convert x')
convert (PNode _ ((PLeaf (Keyword "navigate", s)): x: x': x'': (PNode Block xs): []))	= ForNode 	(convert x) (convert x') (convert x'') (map convert xs)
convert (PNode _ ((PLeaf (Keyword "whirlpool", s)): x: (PNode Block xs): []))			= WhileNode (convert x) (map convert xs)
convert (PNode Program (x:(PLeaf (a,s)):(PNode Block xs):[]))							= ZupaNode 	s (map convert xs)
convert (PNode _ ((PLeaf (Idf, "gift")):x:[])) 											= GiftNode 	(convert x)
convert (PNode _ ((PLeaf (Idf, "plunder")):x:[])) 										= PlunderNode (convert x)
convert (PNode _ ((PLeaf (Keyword "ship", _)): (PLeaf (Idf, s)): (PNode FValues xs): (PNode Block xs'): [])) = FuncNode	s (map convert xs) (map convert xs')
convert (PNode _ ((PLeaf (Keyword "flagship", s)): (PNode Block xs): []))				= FuncNode 	s [] (map convert xs)
convert (PNode _ ((PLeaf (Keyword "booty", "booty")): x: _: x':[]))						= FuncValNode (convert x) (convert x')
convert (PNode _ ((PLeaf (Keyword "parrot", s)): x: []))								= PrintNode (convert x)
convert (PNode _ ((PLeaf (Keyword "avast", s)): x: []))									= ReturnNode s (convert x)
convert (PNode Func ((PLeaf (Idf, s)): xs))												= DoFuncNode s (map convert xs)
convert (PNode _ (x: x': []))															= BootyNode (convert x) (convert x')

data Tree = VarNode 	String
		  | BootyNode 	Tree Tree
		  | OpNode 		String Tree Tree
		  | BoolExNode 	BoolEx
		  | GiftNode	Tree
		  | PlunderNode Tree
		  | IfNode		Tree [Tree]
		  | ElseNode	[Tree]
		  | ForNode		Tree Tree Tree [Tree]
		  | WhileNode	Tree [Tree]
		  | FuncNode 	String [Tree] [Tree]
		  | FuncValNode	Tree Tree
		  | PrintNode	Tree
		  | ReturnNode	String Tree
		  | DoFuncNode	String [Tree]
		  | ZupaNode	String [Tree]
		  deriving (Eq, Show)

data BoolEx = Comp String Tree Tree
			| Boolean Tree
			deriving (Eq, Show)

toRTree :: Tree -> RoseTree
toRTree (VarNode s) 				= RoseNode s []
toRTree (BootyNode t1 t2) 			= RoseNode "var" [toRTree t1, toRTree t2]
toRTree (OpNode s t1 t2)			= RoseNode s [toRTree t1, toRTree t2]
toRTree (BoolExNode (Comp s t1 t2)) = RoseNode s [toRTree t1, toRTree t2]
toRTree (BoolExNode (Boolean t1)) 	= RoseNode "boolean" [toRTree t1]
toRTree (GiftNode t1)				= RoseNode "inc" [toRTree t1]
toRTree (PlunderNode t1)			= RoseNode "dec" [toRTree t1]
toRTree (IfNode t1 list)			= RoseNode "if" (map toRTree (t1:list))
toRTree (ElseNode list)				= RoseNode "else" (map toRTree (list))
toRTree (ForNode t1 t2 t3 list)		= RoseNode "for" (map toRTree (t1:t2:t3:list))
toRTree (WhileNode t1 list)			= RoseNode "while" (map toRTree (t1:list))
toRTree (FuncNode s list1 list2)	= RoseNode s (map toRTree (list1 ++ list2))
toRTree (FuncValNode t1 t2)			= RoseNode "val" [toRTree t1, toRTree t2]
toRTree (PrintNode t1)				= RoseNode "print" [toRTree t1]
toRTree (ReturnNode s t1)			= RoseNode s [toRTree t1]
toRTree (DoFuncNode s list)			= RoseNode s (map toRTree list)
toRTree (ZupaNode s list)			= RoseNode s (map toRTree list)

showConvertedTree = showRoseTree $ toRTree $ convert test1
