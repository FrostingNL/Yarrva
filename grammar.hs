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
				[functionKey, idf, lpar, FValues, rpar, Block],													-- Normal Function
				[mainKey, lpar, rpar, Block]]																	-- Main Function
	Array   -> [[arrayKey, Type, idf, NoCat equalsKey, lbra, ArrayList, rbra, NoCat endmark]]
	Block	-> [[lcbr, Rep0 [Stat], rcbr]]																		-- A block of code
	BoolExpr-> [[Expr, Alt [equalsKey] [Alt [Alt [lesserKey] [lequalsKey]] [Alt [greaterKey] [gequalsKey]]], Expr],									-- A boolean expression
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
				[SyntCat String],																				-- A string
				[Func]]
	Bool  	-> [[Alt [trueKey] [falseKey]]]
	ArrayList->[[Alt [Type] [idf], Rep0 [comma, Alt [Type] [idf] ] ]]
	FValues -> [[Opt [FuncVal, Rep0 [NoCat comma, FuncVal]]]]
	FuncVal	-> [[Var, idf]]																						-- The variable you can use in a function decleration
	Assign 	-> [[Opt [Var], idf, NoCat equalsKey, Expr]]														-- Assign decleration
	Func 	-> [[idf, lpar, Opt [Expr, Rep0 [NoCat comma, Expr]], rpar]]										-- Function
	Var 	-> [[intKey],
				[boolKey],
				[stringKey]]

data Types 	= Int
			| Boo
			| Str
			| Arr
			| Err
			deriving (Eq,Show)


progKey 	= Keyword "fleet"
functionKey = Keyword "ship"
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
boolKey		= Keyword "bool"
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

data State = START | ERROR | KEY | NUM | SYM | IDF | BOOL | BOOLID | STR | STRID | ARRAY | ARRAYTYPE | ARRAYID | ARRAYELEM

tokenizer :: State -> Int -> Int -> String -> [Token]
tokenizer _ _ _ [] = []
tokenizer s l c (' ':xs) 							= tokenizer s l (c+1) xs
tokenizer s l c ('\t':xs) 						= tokenizer s l (c+3) xs
tokenizer s l c ('\n':xs)							= tokenizer s (l+1) 0 xs
tokenizer s l c ('*':'*':xs) 						= tokenizer s l (c+2) (rmLineComment xs)
tokenizer s l c ('>':'>':xs) 						= tokenizer s l (c+2) (rmBlockComment xs)
tokenizer s l c ('b':'e':xs) 						| checkCompare xs = (getKeyword keyStr, keyStr, l, c) : tokenizer s l (c+8) (rmComp xs True)
												| otherwise = (equalsKey, "be", l, c) : tokenizer s l (c+2) xs
												where 
													keyStr = "be " ++ getCompare xs True
tokenizer s l c (',':' ':'A':'r':'r':'r':'!':xs) 	= (endmark, getEndmark, l, c): tokenizer START l (c+7) xs
tokenizer state l c str@(x:xs) =
	case state of
		ERROR 		->  error ("Shiver me timbers! You done it wrong, Arrr! On line: " ++ (show l) ++ ":" ++ (show c))
		BOOLID		-> (Idf, getWord (str), l, c)   : tokenizer BOOL l (c+ length (getWord str)) (getRest str)
		BOOL 		-> (Bool, getWord str, l, c)    : tokenizer BOOL l (c + length (getWord str)) (getRest str)
		STRID 		-> (Idf, getWord str, l, c)     : tokenizer STR l (c + length (getWord str)) (getRest str)
		ARRAY 		-> (arrayKey, "treasure", l, c) : tokenizer ARRAYTYPE l c str
		ARRAYID 	-> (Idf, getWord str, l, c) 	 : tokenizer ARRAYELEM l (c + length (getWord str)) (getRest str)
		IDF         -> (Idf, getWord str, l, c) 	 : tokenizer START l (c + length (getWord str)) (getRest str)
					-- START STATE
		START 		| isKeyword $ getWord str 	 	-> tokenizer KEY l c str
				 	| isString $ getString str  	-> tokenizer STR l c str
				 	| isNumber x 					-> tokenizer NUM l c str
				 	| isGramSymbol x 				-> tokenizer SYM l c str
				 	| isArray str    				-> tokenizer ARRAY l c str
				 	| otherwise 				    -> tokenizer IDF l c str
					-- SYM STATE
		SYM 		| elem x "+-*/" 				-> (Op, [x], l, c) : tokenizer START l (c+1) xs
			   		| otherwise 					-> (getSymbol x, [x], l, c) : tokenizer START l (c+1) xs
			   		-- NUM STATE
		NUM 		| isNumber x 					-> (Nmbr, getNum str, l, c) : tokenizer START l (c + length (getNum str)) (rmNum str) 
					| otherwise 					-> tokenizer IDF l c str
					-- STR STATE
		STR 		| isString (getString str) 		-> (String, getString str, l, c) : tokenizer START l (c + length (getString str)) (getRest str)
					| otherwise 					-> tokenizer ERROR l c str
					-- KEY STATE
		KEY 		| getKey str == intKey 			-> makeKeyToken str l c : tokenizer NUM l (c + length (getWord str)) (getRest xs)
					| getKey str == boolKey 		-> makeKeyToken str l c : tokenizer BOOLID l (c + length (getWord str)) (getRest xs)
					| getKey str == stringKey 		-> makeKeyToken str l c : tokenizer STRID l (c + length (getWord str)) (getRest xs)
					| otherwise 					-> makeKeyToken str l c : tokenizer START l (c + length (getWord str)) (getRest xs)
					-- ARRAYTYPE STATE
		ARRAYTYPE 	| getAType str == intKey 		-> (Nmbr, getArrayType str, l, c)   : tokenizer ARRAYID l (c + length (getArrayType str)) (getRest xs)
					| getAType str == boolKey 		-> (Bool, getArrayType str, l, c)   : tokenizer ARRAYID l (c + length (getArrayType str)) (getRest xs)
					| getAType str == stringKey 	-> (String, getArrayType str, l, c) : tokenizer ARRAYID l (c + length (getArrayType str)) (getRest xs)
					| otherwise 					-> tokenizer ERROR l c str
					-- ARRAYELEM STATE
		ARRAYELEM   | isGramSymbol x 				-> (getSymbol x, [x], l, c) 	  : tokenizer ARRAYELEM (c+1) l xs
					| isNumber x 					-> (Nmbr, getNum str, l, c) 	  : tokenizer ARRAYELEM (c + length (getNum str)) l (rmNum xs)
					| isBoolean $ getRestW str      -> (Bool, getRestW str, l, c) 	  : tokenizer ARRAYELEM l (c + length (getRestW str)) (rmUpTo xs [',', ']'])
					| otherwise 					-> (String, getString str, l, c) : tokenizer ARRAYELEM l (c + length (getString str)) (rmUpTo xs [',', ']'])


getAType str = getKeyword $ getArrayType str
getKey str = getKeyword $ getWord str
makeKeyToken str l c = (getKeyword $ getWord str, getWord str, l, c)
getRestW str = getUpTo str [',',']'] 

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
rmNum (x:xs) | elem x "1234567890" = rmNum xs
			 | otherwise = (x:xs)

isArray :: String -> Bool
isArray str = elem (getArrayType str) allTypes && contains str '[' && contains str ']'

getArrayType :: String -> String
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
	| x == ' ' = xs
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
				(boolKey, "bool"),
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
				(gequalsKey, "be above")
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
allTypes = ["doubloon", "booty", "bool"]

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
						 "   ship add3(doubloon i) {",
						 "      avast i + 3, Arrr!",
						 "   }",
						 "   ",
						 "   flagship() {",
						 "      doubloon a be add3(5), Arrr!",
						 "      parrot a, Arrr!",
						 "   }",
						 "}"
						]


test = unlines ["fleet Prog {",	
			   "    ship a() {",
				"   }",
				"   doubloon int be 1, Arrr!",
				"   booty c be \"SDSD\", Arrr!",
				"   ship b() {",
				"       bool a be Aye, Arrr!",
				"       doubloon[] array be [1,2,3,4], Arrr!",
				"   }",
				"   parley (int be 1) { }",
			   "}"
			   ]

test2 = unlines ["fleet Program {",	
		"doubloon c be n +m, Arrr!",
		"booty d be \"Hello\", Arrr!",
		"bool h be Aye, Arrr!",
		"doubloon[] a be [1,3,5], Arrr!",
		"}"
		]



test3 = unlines ["fleet Fleet {",
				"    doubloon a be 1, Arrr!",
				"    navigate(doubloon b be 0. b be below 5. gift b) {",
				"        a be b+a, Arrr!",
				"        parrot(\"Aye\"), Arrr!",
				"    }",
				"}"
				]

tokens = tokenizer START 0 0

test0 = parse grammar Program $ tokens test
test1 = parse grammar Program $ tokens test3

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
	(PLeaf (a, s, l, c)) -> VarNode s l
	(PNode _ [PLeaf (a, s, l, c)])																					| s == "Doubloon" 	-> VarNode "Int" l
																												| s == "Booty"		-> VarNode "String" l
																												| otherwise 		-> VarNode s l
	(PNode _ ((PLeaf (Idf, "gift", _, _)):x:[])) 																	-> GiftNode 	(convert x)
	(PNode _ ((PLeaf (Idf, 	"plunder", _, _)):x:[])) 																-> PlunderNode 	(convert x)
	(PNode _ ((PLeaf (Keyword "parrot", s, _, _)): x: []))															-> PrintNode 	(convert x)
	(PNode _ ((PLeaf (Keyword "avast", s, _, _)): x: []))															-> ReturnNode s (convert x)
	(PNode _ ((PNode _ [PLeaf (Keyword "booty", "booty", _, _)]): x: x':[]))										-> BootyNode 	(convert x) (convert x')
	(PNode _ ((PNode _ [PLeaf (Keyword "doubloon", "doubloon", _, _)]): x: x':[]))									-> DoubloonNode (convert x) (convert x')
	(PNode _ ((PNode _ [PLeaf (Keyword "bool", "bool", _, _)]): x: x':[]))											-> BoolNode 	(convert x) (convert x')
	(PNode _ ((PNode _ [PLeaf (Keyword "treasure", "treasure", _, _)]): x: x':[]))									-> TreasureNode (convert x) (convert x')
	(PNode _ (x: (PLeaf (Op,s, _, _)): x': []))																	-> OpNode s 	(convert x) (convert x')
	(PNode _ ((PLeaf (Keyword "parley", s, _, _)): x: (PNode Block xs):x':x'':[]))									-> IfNode 		(convert x) (map convert xs) (convert (PNode Block [x',x'']))
	(PNode _ ((PLeaf (Keyword "whirlpool", s, _, _)): x: (PNode Block xs): []))									-> WhileNode 	(convert x) (map convert xs)
	(PNode _ ((PLeaf (Keyword "treasure", _,_, _)):x:x':(PNode ArrayList vals):[])) 								-> ArrayNode 	(convert x) (convert x') (map convert vals)
	(PNode _ ((PLeaf (Keyword "navigate", s, _, _)): x: x': x'': (PNode Block xs): []))							-> ForNode 		(convert x) (convert x') (convert x'') (map convert xs)
	(PNode _ ((PLeaf (Keyword "ship", _, _, _)): (PLeaf (Idf, s, _, _)): (PNode FValues xs'):(PNode Block xs): [])) 	-> FuncNode s 	(map convert xs') (map convert xs)
	(PNode _ ((PLeaf (Keyword "heave", s, _, _)): (PNode Block xs): []))											-> ElseNode  	(map convert xs)
	(PNode Program (x:(PLeaf (a,s, _, _)):(PNode Block xs):[]))													-> ZupaNode s 	(map convert xs)
	(PNode Func ((PLeaf (Idf, s, _, _)): xs))																		-> DoFuncNode s (map convert xs)
	(PNode _ ((PLeaf (Keyword "flagship", s, _, _)): (PNode Block xs): []))										-> FuncNode s [] (map convert xs)
	(PNode _ ((PNode _ [PLeaf (Keyword "doubloon", "doubloon", _, _)]): x:[]))										-> FuncValNode 	(convert x) (VarNode "Int" 0)
	(PNode _ ((PNode _ [PLeaf (Keyword "bool", "bool", _, _)]): x:[]))												-> FuncValNode 	(convert x) (VarNode "Bool" 0)
	(PNode _ ((PNode _ [PLeaf (Keyword "treasure", "treasure", _, _)]): x:[]))										-> FuncValNode 	(convert x) (VarNode "Array" 0)
	(PNode _ ((PNode _ [PLeaf (Keyword "booty", "booty", _, _)]): x:[]))											-> FuncValNode 	(convert x) (VarNode "String" 0)
	(PNode _ (x: (PLeaf (Keyword "be",s, _, _)): x': []))															-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "below",s, _, _)): x': []))														-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "above",s, _, _)): x': []))														-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "be below",s, _, _)): x': []))													-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ (x: (PLeaf (Keyword "be above",s, _, _)): x': []))													-> BoolExNode $ Comp s (convert x) (convert x')
	(PNode _ ((PNode Bool [x]): []))																			-> BoolExNode $ Boolean (convert x)
	(PNode _ [node])																							-> convert node

data Tree = VarNode 	String Int
		  | BootyNode 	Tree Tree
		  | DoubloonNode Tree Tree
		  | BoolNode 	Tree Tree
		  | TreasureNode Tree Tree
		  | OpNode 		String Tree Tree
		  | BoolExNode 	BoolEx
		  | GiftNode	Tree
		  | PlunderNode Tree
		  | IfNode		Tree [Tree] Tree
		  | ElseNode	[Tree]
		  | ForNode		Tree Tree Tree [Tree]
		  | WhileNode	Tree [Tree]
		  | FuncNode 	String [Tree] [Tree]
		  | FuncValNode	Tree Tree
		  | PrintNode	Tree
		  | ReturnNode	String Tree
		  | DoFuncNode	String [Tree]
		  | ArrayNode   Tree Tree [Tree]
		  | ZupaNode	String [Tree]
		  deriving (Eq, Show)

data BoolEx = Comp String Tree Tree
			| Boolean Tree
			deriving (Eq, Show)

toRTree :: Tree -> RoseTree
toRTree (VarNode s l) 				= RoseNode ((show l) ++ ": " ++ s) []
toRTree (BootyNode t1 t2) 			= RoseNode "strDecl" [toRTree t1, toRTree t2]
toRTree (DoubloonNode t1 t2) 		= RoseNode "intDecl" [toRTree t1, toRTree t2]
toRTree (BoolNode t1 t2) 			= RoseNode "boolDecl" [toRTree t1, toRTree t2]
toRTree (TreasureNode t1 t2)		= RoseNode "arrayDecl" [toRTree t1, toRTree t2]
toRTree (OpNode s t1 t2)			= RoseNode s [toRTree t1, toRTree t2]
toRTree (BoolExNode (Comp s t1 t2)) = RoseNode s [toRTree t1, toRTree t2]
toRTree (BoolExNode (Boolean t1)) 	= RoseNode "boolean" [toRTree t1]
toRTree (GiftNode t1)				= RoseNode "inc" [toRTree t1]
toRTree (PlunderNode t1)			= RoseNode "dec" [toRTree t1]
toRTree (IfNode t1 list l)			= RoseNode "if" (map toRTree (t1 : list ++ [l]))
toRTree (ElseNode list)				= RoseNode "else" (map toRTree (list))
toRTree (ForNode t1 t2 t3 list)		= RoseNode "for" (map toRTree (t1:t2:t3:list))
toRTree (WhileNode t1 list)			= RoseNode "while" (map toRTree (t1:list))
toRTree (FuncNode s list1 list2)	= RoseNode s (map toRTree (list1 ++ list2))
toRTree (FuncValNode t1 t2)			= RoseNode "param" [toRTree t1, toRTree t2]
toRTree (PrintNode t1)				= RoseNode "print" [toRTree t1]
toRTree (ReturnNode s t1)			= RoseNode s [toRTree t1]
toRTree (DoFuncNode s list)			= RoseNode s (map toRTree list)
toRTree (ArrayNode t1 t2 list)		= RoseNode "arrayDecl" ([toRTree t1, toRTree t2] ++ (map toRTree list))
toRTree (ZupaNode s list)			= RoseNode s (map toRTree list)

showConvertedTree = showRoseTree $ toRTree $ convert test1	

--main = file "Example programs/test.yarr"
