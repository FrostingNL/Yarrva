module Grammar where

import Parse
import Data.Char
import Data.List
import Debug.Trace
import FPPrac.Trees
import System.IO

grammar :: Grammar
grammar nt = case nt of 																			-- The Grammar sorted by occurence
	Program -> [[progKey, idf, Block]]																			-- The Main Program
	Stat 	-> [[Opt [Var], idf, NoCat equalsKey, Expr, NoCat endmark], 										-- Var declaration
				[Array],
				[ifExprKey, lpar, BoolExpr, rpar, Block, Opt [elseKey, Block]],									-- IfElse Expression
				[printKey, Expr, NoCat endmark],																-- Print Expression
				[incKey, Type, NoCat endmark],
				[decKey, Type, NoCat endmark],
				[forKey, lpar, Assign, point, BoolExpr, point, Expr, rpar, Block],								-- For Expression
				[whileKey, lpar, BoolExpr, rpar, Block],														-- While Expression
				[Func, endmark],
				[returnKey, Opt [Expr], NoCat endmark],															-- Return Expression
				[Function, idf, lpar, FValues, rpar, Block],													-- Normal Function
				[mainKey, lpar, rpar, Block]]																	-- Main Function
	Array   -> [[arrayKey, Type, idf, NoCat equalsKey, lbra, ArrayList, rbra, NoCat endmark]]
	Block	-> [[lcbr, Rep0 [Stat], rcbr]]																		-- A block of code
	BoolExpr-> [[Expr, Alt [equalsKey] [Alt [Alt [lesserKey] [lequalsKey]] [Alt [greaterKey] [gequalsKey]]], Expr],									-- A boolean expression
				[Bool],																							-- A boolean
				[idf],																							-- An identifier
				[BoolExpr, Alt [orKey] [andKey], BoolExpr]] 													-- Two boolean expressions
	Expr 	-> [[Alt [Expr2] [lpar, Expr2, rpar]]]
	Expr2 	-> [[ArrayOp],	
				[Type, Opt [SyntCat Op, Type]],																		-- An expression
				[incKey, Type],																					-- Increase Type by 1
				[decKey, Type]]																				-- One of the types
	Op		-> [[plus],																							-- Self Explanatory
				[minus],																						-- Self Explanatory
				[times],																						-- Self Explanatory
				[divide],																						-- Self Explanatory
				[notSym]]																						-- Self Explanatory
	Type	-> [[SyntCat Nmbr],																					-- A number
				[SyntCat Bool],																					-- A boolean
				[idf],																							-- An identifier
				[SyntCat String],																				-- A string
				[Func]]
	ArrayOp	-> [[idf, lbra, ArrayIndex, rbra]]
	ArrayIndex->[[Alt [SyntCat Nmbr] [idf]]]
	Bool  	-> [[Alt [trueKey] [falseKey]]]
	ArrayList->[[Alt [Type] [idf], Rep0 [comma, Alt [Type] [idf] ] ]]
	FValues -> [[Opt [FuncVal, Rep0 [NoCat comma, FuncVal]]]]
	FuncVal	-> [[Var, idf]]																						-- The variable you can use in a function decleration
	Assign 	-> [[Opt [Var], idf, NoCat equalsKey, Expr]]														-- Assign decleration
	Func 	-> [[idf, lpar, Opt [Expr, Rep0 [NoCat comma, Expr]], rpar]]										-- Function
	Var 	-> [[intKey],
				[boolKey],
				[stringKey]]
	Function-> [[intFunction], 
				[boolFunction],
				[strFunction],
				[arrFunction]]

data Types 	= Int
			| Boo
			| Str
			| Arr Types
			| Fun
			| Err
			deriving (Eq,Show)


progKey 	= Keyword "fleet"
functionKey = Keyword "ship"
intFunction = Keyword "doubloonShip"
boolFunction= Keyword "orderShip"
strFunction	= Keyword "bootyShip"
arrFunction = Keyword "treasureShip"
mainKey		= Keyword "flagship"
returnKey 	= Keyword "avast"
equalsKey 	= Keyword "be"	
lesserKey	= Keyword "below"
greaterKey	= Keyword "above"
lequalsKey  = Keyword "be below"
gequalsKey  = Keyword "be above"
trueKey 	= Keyword "Aye"
falseKey 	= Keyword "Nay"
intKey 		= Keyword "doubloon"
boolKey		= Keyword "order"
stringKey	= Keyword "booty"
arrayKey	= Keyword "treasure"
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

data State = START | ERROR | KEY | NUM | SYM | IDF | BOOL | BOOLID | STR | STRID | ARRAY | ARRAYTYPE | ARRAYID | ARRAYELEM | ARRAYOP | ARRAYOP' | ARRAYOP''

tokenizer :: State -> Int -> Int -> String -> [Token]
tokenizer _ _ _ [] = []
tokenizer s l c (' ':xs) 						= tokenizer s l 	(c+1) xs
tokenizer s l c ('\t':xs) 						= tokenizer s l 	(c+3) xs
tokenizer s l c ('\n':xs)						= tokenizer s (l+1) 0 	  xs
tokenizer s l c ('*':'*':xs) 					= tokenizer s l 	(c+2) (rmLineComment xs)
tokenizer s l c ('>':'>':xs) 					= tokenizer s l 	(c+2) (rmBlockComment xs)
tokenizer s l c ('b':'e':xs) 					| checkCompare xs = (getKeyword keyStr, keyStr, l, c) : tokenizer s l (c+8) (rmComp xs True)
												| otherwise = (equalsKey, "be", l, c) : tokenizer s l (c+2) xs
												where 
													keyStr = "be " ++ getCompare xs True
tokenizer s l c (',':' ':'A':'r':'r':'r':'!':xs) 	= (endmark, getEndmark, l, c): tokenizer START l (c+7) xs
tokenizer state l c str@(x:xs) =
	case state of
		ERROR 		->  error ("Shiver me timbers! You done it wrong, Arrr! On line: " ++ (show l) ++ ":" ++ (show c))
		BOOLID		-> (Idf, getWord (str), l, c)   : tokenizer BOOL 		l (calcC c str) (getRest str)
		STRID 		-> (Idf, getWord str, l, c)     : tokenizer STR 		l (calcC c str) (getRest str)
		ARRAY 		-> (arrayKey, "treasure", l, c) : tokenizer ARRAYTYPE 	l c str
		ARRAYID 	-> (Idf, getWord str, l, c) 	: tokenizer ARRAYELEM 	l (calcC c str) (getRest str)
		IDF         -> (Idf, getWord str, l, c) 	: tokenizer START 		l (calcC c str) (getRest str)
		ARRAYOP     -> (Idf, getIdf str, l, c) 		: tokenizer ARRAYOP' 	l (c + length (getIdf str)) (rmIdf xs)
		ARRAYOP'	| x == '[' 						-> symbolToken : tokenizer ARRAYOP'' l (c+1) xs
					| x == ']'						-> symbolToken : tokenizer START l (c+1) xs
				    | otherwise						-> tokenizer ERROR l c str
				    where 
				    	symbolToken = (getSymbol x, [x], l, c)
		ARRAYOP''	| isNumber x 					-> (Nmbr, num, l, c) : tokenizer ARRAYOP' l (c + length num) (rmNum str)
					| isLowercase x 				-> (Idf, idf, l, c) : tokenizer ARRAYOP' l (c + length idf) (rmIdf str)
					| otherwise 					-> tokenizer ERROR l c str
					where
						idf = getIdf str
						num = getNum str
					-- START STATE
		START 		| isKeyword $ getWord str 	 	-> tokenizer KEY 	 l c str
				 	| isString $ getString str  	-> tokenizer STR 	 l c str
				 	| isNumber x 					-> tokenizer NUM 	 l c str
				 	| isGramSymbol x 				-> tokenizer SYM 	 l c str
				 	| isArray str    				-> tokenizer ARRAY 	 l c str
				 	| isArrayOp $ getWord str 		-> tokenizer ARRAYOP l c str
				 	| otherwise 				    -> tokenizer IDF 	 l c str
					-- SYM STATE
		SYM 		| elem x "+-*/" 				-> (Op, [x], l, c) 		    : tokenizer START l (c+1) xs
			   		| otherwise 					-> (getSymbol x, [x], l, c) : tokenizer START l (c+1) xs
			   		-- BOOL STATE
		BOOL 		| isBoolean bool 				-> (Bool, bool, l, c)    : tokenizer BOOL 		l (calcC c str) (getRest str)
				    | otherwise 					-> tokenizer START l c str
				    where
				    	bool = getWord str
			   		-- NUM STATE
		NUM 		| isNumber x 					-> (Nmbr, getNum str, l, c) : tokenizer START l newC rest 
					| otherwise 					-> tokenizer IDF l c str
					where
						newC = c + length (getNum str)
						rest = rmNum str
					-- STR STATE
		STR 		| isString (getString str) 		-> (String, getString str, l, c) : tokenizer START l newC rest
					| otherwise 					-> tokenizer ERROR l c str
					where
						newC = c + length (getString str)
						rest = getRest str
					-- KEY STATE
		KEY 		| keyword == intKey 						-> newToken 	      : tokenizer NUM l newC rest
					| keyword == boolKey                     	-> newToken 		  : tokenizer BOOLID l newC rest
					| keyword == stringKey 						-> newToken 		  : tokenizer STRID l newC rest
					| keyword == trueKey || keyword == falseKey -> (Bool, word, l, c) : tokenizer START l newC rest
					| otherwise 								-> newToken 		  : tokenizer START l newC rest
					where 
						word = getWord str
						newC = c + length word
						rest = getRest xs
						keyword = getKeyword word
						newToken = (keyword, word, l, c)
					-- ARRAYTYPE STATE
		ARRAYTYPE 	| arrayKey == intKey 		-> (Nmbr, arrayType, l, c)   : nextState
					| arrayKey == boolKey 		-> (Bool, arrayType, l, c)   : nextState
					| arrayKey == stringKey 	-> (String, arrayType, l, c) : nextState
					| otherwise 				-> tokenizer ERROR l c str
					where
						nextState = tokenizer ARRAYID l newC rest
						arrayType = getArrayType str
						arrayKey = getKeyword arrayType
						newC = c + length arrayType
						rest = getRest xs
					-- ARRAYELEM STATE
		ARRAYELEM   | isGramSymbol x 				-> (getSymbol x, [x], l, c): tokenizer ARRAYELEM (c+1) l xs
					| isNumber x 					-> (Nmbr, num, l, c) 	   : tokenizer ARRAYELEM l (c + length num) (rmNum xs)
					| isBoolean restW 			    -> (Bool, restW, l, c) 	   : tokenizer ARRAYELEM l (c + length restW) rest
					| otherwise 					-> (String, string, l, c)  : tokenizer ARRAYELEM l (c + length string) rest
					where 
						rest = rmUpTo xs [',', ']']
						restW = getUpTo str [',',']']
						string = getString str
						num = getNum str

calcC c str = c + length (getWord str)

isArrayOp :: String -> Bool
isArrayOp [] = False
isArrayOp (x:xs) = x /= '[' &&  contains (x:xs) '[' && contains (x:xs) ']'


checkCompare :: String -> Bool
checkCompare str = isCompKey $ getCompare str True

getCompare :: String -> Bool -> String
getCompare [] _ = []
getCompare (' ':xs) True = getCompare xs True
getCompare (' ':xs) False = []
getCompare (x:xs) _ | isGramSymbol x = getCompare xs False
				    | otherwise = x : getCompare xs False

rmComp :: String -> Bool -> String
rmComp [] _ = []
rmComp (' ':xs) True = rmComp xs True
rmComp (' ':xs) False = (' ':xs)
rmComp (x:xs) _ | isGramSymbol x = (x:xs)
				| otherwise = rmComp xs False

rmLineComment :: String -> String
rmLineComment [] = []
rmLineComment ('\n':xs) = ('\n':xs)
rmLineComment (_:xs) = rmLineComment xs

rmBlockComment :: String -> String
rmBlockComment [] = []
rmBlockComment ('<':'<':xs) = xs
rmBlockComment (_:xs) = rmBlockComment xs

rmNum :: String -> String
rmNum [] = []
rmNum (x:xs) | isNumber x = rmNum xs
			 | otherwise = (x:xs)

isArray :: String -> Bool
isArray str = elem (getArrayType str) allTypes && contains str '[' && contains str ']'

getArrayType :: String -> String
getArrayType [] = ""
getArrayType (x:xs) 
	| x /= '[' = x : getArrayType xs
	| otherwise = []

contains :: String -> Char -> Bool
contains [] _ = False
contains (x:xs) a | x == a = True
				  | otherwise = contains xs a

getArray :: String -> String
getArray [] = []
getArray ('[':xs) = "[" ++ getArray xs
getArray (']':xs) = "]"
getArray (x:xs) = x : getArray xs

isLowercase :: Char -> Bool
isLowercase x = ord x >= 97 && ord x <= 122

getString :: String -> String
getString [] 			= []
getString [x] 			= [x]
getString ('\\':'"':xs) = '\\': '"': getString (xs)
getString (x:'"':xs)	= [x, '"']
getString (x:xs)		= x: getString xs

getRestString :: String -> String
getRestString [] = []
getRestString [x] = []
getRestString (x:x':xs)
	| x == '\\' && x' == '"' = getRestString xs
	| x /= '\\' && x' == '"' = xs
	| otherwise				 = getRestString xs

isString :: String -> Bool
isString (x:xs) = x == '"' && (xs!!((length xs)-1)) == '"'

rmEndMark :: String -> String
rmEndMark [] = []
rmEndMark (',':' ':'A':'r':'r':'r':'!':xs) = xs

isGramSymbol :: Char -> Bool
isGramSymbol s = elem s [snd y | y <- allSymbols]

getSymbol :: Char -> Alphabet
getSymbol x = [fst tup | tup <- allSymbols, snd tup == x]!!0

isKeyword :: String -> Bool
isKeyword s = elem s [snd y | y <- allKeywords ++ compareKeys]

getKeyword :: String -> Alphabet
getKeyword x = [fst tup | tup <- allKeywords ++ compareKeys, snd tup == x]!!0

isCompKey :: String -> Bool
isCompKey s = elem s [snd y | y <- compareKeys]

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

getIdf :: String -> String
getIdf [] = []
getIdf (x:xs) | elem x (" +-*/,().[]") = []
			  | otherwise = x : getIdf xs

rmIdf :: String -> String
rmIdf [] = []
rmIdf (x:xs) | elem x (" +-*/,().[]") = (x:xs)
			 | otherwise = rmIdf xs

getUpTo :: String -> [Char] -> String
getUpTo [] _ = []
getUpTo (s:str) chars | elem s chars = []
				      | otherwise = s : getUpTo str chars

rmUpTo :: String -> [Char] -> String
rmUpTo [] _ = []
rmUpTo (s:str) chars | elem s chars = (s:str)
					 | otherwise = rmUpTo str chars

getRest :: String -> String
getRest [] = []
getRest (x:xs)
	| elem x ",+-*/()." = (x:xs)
	| x == ' ' = (x:xs)
	| otherwise = getRest xs

getEndmark :: String
getEndmark = ", Arrr!"

allKeywords :: [(Alphabet, String)]
allKeywords = [(progKey, "fleet"),
				(functionKey, "ship"),
				(mainKey, "flagship"),
				(returnKey, "avast"),
				(trueKey, "Aye"),
				(falseKey, "Nay"),
				(intKey, "doubloon"),
				(boolKey, "order"),
				(stringKey, "booty"),
				(arrayKey, "treasure"),
				(ifExprKey, "parley"),
				(equalsKey, "be"),
				(elseKey, "heave"),
				(breakKey, "belay"),
				(printKey, "parrot"),
				(continueKey, "God's speed"),
				(whileKey, "whirlpool"),
				(forKey, "navigate"),
				(orKey, "or"),
				(andKey, "'n"),
				(incKey, "gift"),
				(decKey, "plunder"),
				(endmark, ", Arrr!"),
				(lequalsKey, "be below"),
				(gequalsKey, "be above"),
				(intFunction, "doubloonShip"),
				(boolFunction, "orderShip"),
				(arrFunction, "treasureShip"),
				(strFunction, "bootyShip")
				]

compareKeys :: [(Alphabet, String)]
compareKeys =  [(lesserKey, "below"),
				(greaterKey, "above")
			   ]


allSymbols :: [(Alphabet, Char)]
allSymbols = [ (lpar, '('),
				(rpar, ')'),
				(lbra, '['),
				(rbra, ']'),
				(lcbr, '{'),
				(rcbr, '}'),
				(eq, '='),
				(lt, '<'),
				(ge, '>'),
				(plus, '+'),
				(minus, '-'),
				(times,  '*'),
				(divide,  '/'),
				(notSym,  '~'),
				(colon,  ':'),
				(point,  '.'),
				(comma, ',')
			]



allTypes :: [String]
allTypes = ["doubloon", "booty", "order"]

startsWith :: String -> String -> Bool
startsWith [] _ 	= True
startsWith _ [] 	= False
startsWith (s:search) (w:word) 
	| s == w = startsWith search word
	| otherwise = False

helloWorld = unlines ["fleet HelloWorld {",
					 "   parrot \"Ahoy World!\", Arrr!",
					 "}"
					]

sampleFunction = unlines ["fleet SampleFunction {",
						 "   doubloon boob be 4, Arrr!",
						 "   order boooob be Aye, Arrr!",
						 "   ship add3(doubloon i) {",
						 "      avast i + 3, Arrr!",
						 "   }",
						 "   ",
						 "   flagship() {",
						 "      doubloon a be add3(5), Arrr!",
						 "      parrot a, Arrr!",
						 "   }",
						 "   navigate(doubloon a be 0. a be below 5. gift a) {",
						 "       parley(a be 1) {",
						 "           doubloon cups be 10, Arrr!",
						 "           parley(5 be below cups) {",
						 "                cups be 5, Arrr!",
						 "           }",
						 "           parrot (a), Arrr!",
						 "       }",
						 "   }",
						 "}"
						]


test = unlines ["fleet Prog {",	
			   	"    doubloonShip a(doubloon i) {",
			   	"        i be i + 1, Arrr!",
			   	"        avast i, Arrr!",
			   	"    }",
			   	"    flagship() {",
			   	"        doubloon b be a(1), Arrr!",
			   	"        parrot b, Arrr!",
			   	"    }",
			   "}"
			   ]

test2 = unlines ["fleet Program {",	
		"doubloon[] a be [1,3,5], Arrr!",
		"doubloon c be a[2], Arrr!",
		"}"
		]

test3 = unlines ["fleet Fleet {",
	"doubloonShip a(doubloon b, order c) {",
	"}",
      "}"]

tokens = tokenizer START 0 0

test0 = parse grammar Program $ tokens test
test1 = parse grammar Program $ tokens test2

showTestTree = showRoseTree $ toRoseTree1 test0
showTestTree2 = showRoseTree $ toRoseTree1 test1

printTupList :: [(Alphabet, String, Int, Int)] -> IO String
printTupList [t] = do return (show t)
printTupList (t:tup) = do
						putStrLn (show t)
						printTupList tup

fileList :: FilePath -> IO ()
fileList f = do  
	handle <- openFile f ReadMode  
	contents <- hGetContents handle
	showRoseTreeList [toRoseTree1 $ parse grammar Program $ tokens contents, toRTree $ convert $ parse grammar Program $ tokens contents]

file :: FilePath -> IO ()
file f = do  
	handle <- openFile f ReadMode  
	contents <- hGetContents handle
	printTupList $ tokens contents
	showRoseTree $ toRTree $ convert $ parse grammar Program $ tokens contents

convert :: ParseTree -> Tree
convert tree = case tree of 
	(PLeaf (a, s, l, c)) -> VarNode s l c
	(PNode _ [PLeaf (a, s, l, c)])																					| s == "Doubloon" 	-> VarNode "Int" l c
																													| s == "Booty"		-> VarNode "String" l c
																													| otherwise 		-> VarNode s l c
	(PNode _ ((PLeaf (Keyword "gift", _, _, _)):x:[])) 																-> GiftNode 	(convert x)
	(PNode _ ((PLeaf (Keyword "plunder", _, _, _)):x:[])) 															-> PlunderNode 	(convert x)
	(PNode _ ((PLeaf (Keyword "parrot", s, _, _)): x: []))															-> PrintNode 	(convert x)
	(PNode _ ((PLeaf (Keyword "avast", s, _, _)): x: []))															-> ReturnNode s (convert x)
	(PNode _ ((PNode _ [PLeaf (Keyword "booty", "booty", _, _)]): x: x':[]))										-> BootyNode 	(convert x) (convert x')
	(PNode _ ((PNode _ [PLeaf (Keyword "doubloon", "doubloon", _, _)]): x: x':[]))									-> DoubloonNode (convert x) (convert x')
	(PNode _ ((PNode _ [PLeaf (Keyword "order", "order", _, _)]): x: x':[]))										-> BoolNode 	(convert x) (convert x')
	(PNode _ ((PNode _ [PLeaf (Keyword "treasure", "treasure", _, _)]): x: x':[]))									-> TreasureNode (convert x) (convert x')
	(PNode _ (PLeaf (Idf, s, l, c): x: []))																			-> AssignNode 	(VarNode s l c) (convert x)
	(PNode _ (x: (PLeaf (Op,s, _, _)): x': []))																		-> OpNode s 	(convert x) (convert x')
	(PNode _ ((PLeaf (Keyword "parley", s, _, _)): x: (PNode Block xs):[]))											-> IfNode 		(convert x) (map convert xs)
	(PNode _ ((PLeaf (Keyword "parley", s, _, _)): x: (PNode Block xs):x':x'':[]))									-> IfElseNode   (convert x) (map convert xs) (convert (PNode Block [x',x'']))
	(PNode _ ((PLeaf (Keyword "whirlpool", s, _, _)): x: (PNode Block xs): []))										-> WhileNode 	(convert x) (map convert xs)
	(PNode _ ((PLeaf (Keyword "treasure", _,_, _)):x:x':(PNode ArrayList vals):[])) 								-> ArrayNode 	(convert x) (convert x') (map convert vals)
	(PNode _ ((PLeaf (Keyword "navigate", s, _, _)): x: x': x'': (PNode Block xs): []))								-> ForNode 		(convert x) (convert x') (convert x'') (map convert xs)
	(PNode _ ((PNode _ [PLeaf (k, _, _, _)]): (PLeaf (Idf, s, _, _)): (PNode FValues xs'):(PNode Block xs): [])) 	| k == intFunction	-> IntFuncNode s 	(map convert xs') (map convert xs)
																													| k == boolFunction	-> BoolFuncNode s 	(map convert xs') (map convert xs)
																													| k == strFunction	-> StrFuncNode s 	(map convert xs') (map convert xs)
																													| otherwise		  	-> ArrFuncNode s 	(map convert xs') (map convert xs)
	(PNode _ ((PLeaf (Keyword "heave", s, _, _)): (PNode Block xs): []))											-> ElseNode  	(map convert xs)
	(PNode Program (x:(PLeaf (a,s, _, _)):(PNode Block xs):[]))														-> ZupaNode s 	(map convert xs)
	(PNode _ ((PNode Func ((PLeaf (Idf, s, _, _)): xs)):_))															-> DoFuncNode s (map convert xs)
	(PNode _ ((PLeaf (Keyword "flagship", s, _, _)): (PNode Block xs): []))											-> FuncNode s [] (map convert xs)
	(PNode _ ((PNode _ [PLeaf (Keyword "doubloon", "doubloon", _, _)]): x:[]))										-> FuncValNode 	(convert x) (VarNode "Int" 0 0)
	(PNode _ ((PNode _ [PLeaf (Keyword "order", "order", _, _)]): x:[]))											-> FuncValNode 	(convert x) (VarNode "Bool" 0 0)
	(PNode _ ((PNode _ [PLeaf (Keyword "treasure", "treasure", _, _)]): x:[]))										-> FuncValNode 	(convert x) (VarNode "Array" 0 0)
	(PNode _ ((PNode _ [PLeaf (Keyword "booty", "booty", _, _)]): x:[]))											-> FuncValNode 	(convert x) (VarNode "String" 0 0)
	(PNode _ ((PNode ArrayOp [x, PNode ArrayIndex [x']]):[]))													    -> ArrayOpNode 	(convert x) (convert x')							
	(PNode _ (x: (PLeaf (Keyword "be",s, _, _)): x': []))															-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "below",s, _, _)): x': []))														-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "above",s, _, _)): x': []))														-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "be below",s, _, _)): x': []))														-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "be above",s, _, _)): x': []))														-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ ((PNode Bool [x]): []))																				-> BoolExNode $ Boolean (convert x)
	(PNode _ [node])																								-> convert node

data Tree = VarNode 	String Int Int
		  | BootyNode 	Tree Tree
		  | DoubloonNode Tree Tree
		  | BoolNode 	Tree Tree
		  | TreasureNode Tree Tree
		  | AssignNode	Tree Tree
		  | OpNode 		String Tree Tree
		  | BoolExNode 	BoolEx
		  | GiftNode	Tree
		  | PlunderNode Tree
		  | IfNode		Tree [Tree]
		  | IfElseNode 	Tree [Tree] Tree
		  | ElseNode	[Tree]
		  | ForNode		Tree Tree Tree [Tree]
		  | WhileNode	Tree [Tree]
		  | FuncNode 	String [Tree] [Tree]
		  | BoolFuncNode 	String [Tree] [Tree]
		  | StrFuncNode 	String [Tree] [Tree]
		  | IntFuncNode 	String [Tree] [Tree]
		  | ArrFuncNode 	String [Tree] [Tree]
		  | FuncValNode	Tree Tree
		  | PrintNode	Tree
		  | ReturnNode	String Tree
		  | DoFuncNode	String [Tree]
		  | ArrayNode   Tree Tree [Tree]
		  | ZupaNode	String [Tree]
		  | ArrayOpNode Tree Tree
		  deriving (Eq, Show)

data BoolEx = Comp String Tree Tree
			| Boolean Tree
			deriving (Eq, Show)

toRTree :: Tree -> RoseTree
toRTree (VarNode s l c) 			= RoseNode ((show l) ++ ":" ++ (show c) ++ ":" ++ s) []
toRTree (ArrayOpNode t1 t2)			= RoseNode "arrayOp" [toRTree t1, toRTree t2]
toRTree (BootyNode t1 t2) 			= RoseNode "strDecl" [toRTree t1, toRTree t2]
toRTree (DoubloonNode t1 t2) 		= RoseNode "intDecl" [toRTree t1, toRTree t2]
toRTree (BoolNode t1 t2) 			= RoseNode "boolDecl" [toRTree t1, toRTree t2]
toRTree (TreasureNode t1 t2)		= RoseNode "arrayDecl" [toRTree t1, toRTree t2]
toRTree (AssignNode s t1)			= RoseNode "assign" [toRTree s, toRTree t1] 
toRTree (OpNode s t1 t2)			= RoseNode s [toRTree t1, toRTree t2]
toRTree (BoolExNode (Comp s t1 t2)) = RoseNode s [toRTree t1, toRTree t2]
toRTree (BoolExNode (Boolean t1)) 	= RoseNode "boolean" [toRTree t1]
toRTree (GiftNode t1)				= RoseNode "inc" [toRTree t1]
toRTree (PlunderNode t1)			= RoseNode "dec" [toRTree t1]
toRTree (IfNode t1 list)			= RoseNode "if"	 (map toRTree (t1:list))
toRTree (IfElseNode t1 list l)		= RoseNode "ifelse" (map toRTree (t1 : list ++ [l]))
toRTree (ElseNode list)				= RoseNode "else" (map toRTree (list))
toRTree (ForNode t1 t2 t3 list)		= RoseNode "for" (map toRTree (t1:t2:t3:list))
toRTree (WhileNode t1 list)			= RoseNode "while" (map toRTree (t1:list))
toRTree (FuncNode s list1 list2)	= RoseNode s (map toRTree (list1 ++ list2))
toRTree (StrFuncNode s list1 list2)	= RoseNode ("strFunc " ++ s) (map toRTree (list1 ++ list2))
toRTree (IntFuncNode s list1 list2)	= RoseNode ("intFunc " ++ s) (map toRTree (list1 ++ list2))
toRTree (BoolFuncNode s list1 list2)= RoseNode ("boolFunc " ++ s) (map toRTree (list1 ++ list2))
toRTree (ArrFuncNode s list1 list2)	= RoseNode ("arrFunc " ++ s) (map toRTree (list1 ++ list2))
toRTree (FuncValNode t1 t2)			= RoseNode "param" [toRTree t1, toRTree t2]
toRTree (PrintNode t1)				= RoseNode "print" [toRTree t1]
toRTree (ReturnNode s t1)			= RoseNode s [toRTree t1]
toRTree (DoFuncNode s list)			= RoseNode s (map toRTree list)
toRTree (ArrayNode t1 t2 list)		= RoseNode "arrayDecl" ([toRTree t1, toRTree t2] ++ (map toRTree list))
toRTree (ZupaNode s list)			= RoseNode s (map toRTree list)

showConvertedTree = showRoseTree $ toRTree $ convert test1

--main = file "Example programs/test.yarr"
