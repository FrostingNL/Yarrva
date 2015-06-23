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
	Array   -> [[arrayKey, Type, idf, NoCat equalsKey, lbra, ArrayList, rbra, NoCat endmark]]
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
				[SyntCat String],																				-- A string
				[Func]]
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

tokenizer :: State -> Int -> String -> [Token]
tokenizer _ _ [] = []
tokenizer s l (' ':xs) = tokenizer s l xs
tokenizer s l ('\t':xs) = tokenizer s l xs
tokenizer s l ('\n':xs) = tokenizer s (l+1) xs

tokenizer ERROR l _ = error ("Shiver me timbers! You done it wrong, Arrr! On line: " ++ (show l))
tokenizer state l ('*':'*':xs) = tokenizer state l (rmLineComment xs)
tokenizer state l ('>':'>':xs) = tokenizer state l (rmBlockComment xs)
tokenizer state l ('b':'e':xs) = (equalsKey, "be", l) : tokenizer state l xs
tokenizer state l (',':' ':'A':'r':'r':'r':'!':xs) = (endmark, getEndmark, l): tokenizer START l xs
tokenizer START l (x:xs) | isKeyword $ getWord (x:xs) 	= tokenizer KEY l (x:xs)
						 | isString (getString (x:xs))  = tokenizer STR l (x:xs)
						 | isNumber x 					= tokenizer NUM l (x:xs)
						 | isGramSymbol x 				= tokenizer SYM l (x:xs)
						 | isArray (x:xs)				= tokenizer ARRAY l (x:xs)
						 | otherwise 				    = tokenizer IDF l (x:xs)
tokenizer SYM l (x:xs) | elem x "+-*/" = (Op, [x], l) : tokenizer START l xs
					   | otherwise = (getSymbol x, [x], l) : tokenizer START l xs
tokenizer KEY l (x:xs) | keyword == intKey = newToken : tokenizer NUM l restString
					   | keyword == boolKey = newToken : tokenizer BOOLID l restString
					   | keyword == stringKey = newToken : tokenizer STRID l restString
					   | otherwise = newToken : tokenizer START l restString
						 where 
						 	restString = getRest xs
						 	newToken = (keyword, restWord, l)
						 	keyword = getKeyword restWord
						 	restWord = getWord (x:xs)
tokenizer BOOLID l str = (Idf, getWord str, l) : tokenizer BOOL l (getRest str)
tokenizer BOOL l str = (Bool, getWord str, l) : tokenizer BOOL l (getRest str)
tokenizer STRID l str = (Idf, getWord str, l) : tokenizer STR l (getRest str)
tokenizer STR l str | isString (getString str) = (String, getString str, l) : tokenizer START l (getRest str)
					| otherwise = tokenizer ERROR l str
tokenizer ARRAY l str = (arrayKey, "treasure", l) : tokenizer ARRAYTYPE l str
tokenizer ARRAYTYPE l (x:xs) | keyword == intKey = (Nmbr, restWord, l) : continueArray
							 | keyword == boolKey = (Bool, restWord, l) : continueArray
							 | keyword == stringKey = (String, restWord, l) : continueArray
							 | otherwise = tokenizer ERROR l (x:xs)
							 where 
							 	restString = getRest xs
							 	newToken = (keyword, restWord, l)
							 	keyword = getKeyword restWord
							 	restWord = getArrayType (x:xs)
							 	continueArray = tokenizer ARRAYID l restString
tokenizer ARRAYID l str = (Idf, getWord str, l) : tokenizer ARRAYELEM l (getRest str)
tokenizer NUM l str | isNumber (str!!0) = (Nmbr, getNum str, l) : tokenizer START l (rmNum str) 
					| otherwise = (Idf, getWord str, l) : tokenizer START l (getRest str)
tokenizer IDF l str = (Idf, getWord str, l) : tokenizer START l (getRest str)
tokenizer ARRAYELEM l (x:xs)| isGramSymbol x = (getSymbol x, [x], l) : tokenizer ARRAYELEM l xs
							| isNumber x = (Nmbr, getNum str, l) : tokenizer ARRAYELEM l (rmNum xs)
							| isBoolean restWord = (Bool, restWord, l) : tokenizer ARRAYELEM l restString
							| otherwise = (String, getString str, l) : tokenizer ARRAYELEM l restString
							where 
								str = (x:xs)
								restWord = getUpTo (x:xs) [',', ']']
								restString = rmUpTo xs [',', ']']

rmLineComment :: String -> String
rmLineComment [] = []
rmLineComment ('\n':xs) = xs
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
isKeyword s = elem s [snd y | y <- allKeywords]

getKeyword :: String -> Alphabet
getKeyword x = [fst tup | tup <- allKeywords, snd tup == x]!!0

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
				(equalsKey, "be"),
				(lesserKey, "below"),
				(greaterKey, "above"),
				(trueKey, "Aye"),
				(falseKey, "Nay"),
				(intKey, "doubloon"),
				(boolKey, "bool"),
				(stringKey, "booty"),
				(arrayKey, "treasure"),
				(ifExprKey, "parley"),
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
				(endmark, ", Arrr!")]

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
				"    doubloon b be 2, Arrr!",
				"    doubloon c be a+b, Arrr!",
				"}"
				]

tokens = tokenizer START 0

test0 = parse grammar Program $ tokens test
test1 = parse grammar Program $ tokens test3

showTestTree = showRoseTree $ toRoseTree1 test0
showTestTree2 = showRoseTree $ toRoseTree1 test1

printTupList :: [(Alphabet, String, Int)] -> IO String
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
	showRoseTree $ toRTree $ convert $ parse grammar Program $ tokens contents

convert :: ParseTree -> Tree

convert (PLeaf (a, s, l)) = VarNode s l
convert (PNode _ ((PNode _ [PLeaf (Keyword "booty", "booty", _)]): x: x':[]))			= BootyNode (convert x) (convert x')
convert (PNode _ ((PNode _ [PLeaf (Keyword "doubloon", "doubloon", _)]): x: x':[]))		= DoubloonNode (convert x) (convert x')
convert (PNode _ ((PNode _ [PLeaf (Keyword "bool", "bool", _)]): x: x':[]))				= BoolNode (convert x) (convert x')
convert (PNode _ ((PNode _ [PLeaf (Keyword "treasure", "treasure", _)]): x: x':[]))		= TreasureNode (convert x) (convert x')
convert (PNode _ (x: (PLeaf (Op,s, _)): x': []))										= OpNode 	s (convert x) (convert x')
convert (PNode _ [PLeaf (a, s, l)])														| s == "Doubloon" 	= VarNode "Int" l
																						| s == "Booty"		= VarNode "String" l
																						| otherwise 		= VarNode s l
convert (PNode _ [node])																= convert node
convert (PNode _ ((PLeaf (Keyword "parley", s, _)): x: (PNode Block xs): []))			= IfNode 	(convert x) (map convert xs)
convert (PNode _ ((PLeaf (Keyword "heave", s, _)): (PNode Block xs): []))				= ElseNode  (map convert xs)
convert (PNode _ (x: (PLeaf (Keyword "be",s, _)): x': []))								= BoolExNode $ Comp s (convert x) (convert x')
convert (PNode _ (x: (PLeaf (Keyword "below",s, _)): x': []))							= BoolExNode $ Comp s (convert x) (convert x')
convert (PNode _ (x: (PLeaf (Keyword "above",s, _)): x': []))							= BoolExNode $ Comp s (convert x) (convert x')
convert (PNode _ ((PLeaf (Keyword "navigate", s, _)): x: x': x'': (PNode Block xs): []))= ForNode 	(convert x) (convert x') (convert x'') (map convert xs)
convert (PNode _ ((PLeaf (Keyword "whirlpool", s, _)): x: (PNode Block xs): []))		= WhileNode (convert x) (map convert xs)
convert (PNode Program (x:(PLeaf (a,s, _)):(PNode Block xs):[]))						= ZupaNode 	s (map convert xs)
convert (PNode _ ((PLeaf (Idf, "gift", _)):x:[])) 										= GiftNode 	(convert x)
convert (PNode _ ((PLeaf (Idf, "plunder", _)):x:[])) 									= PlunderNode (convert x)
convert (PNode _ ((PLeaf (Keyword "ship", _, _)): (PLeaf (Idf, s, _)): (PNode FValues xs): (PNode Block xs'): [])) = FuncNode	s (map convert xs) (map convert xs')
convert (PNode _ ((PLeaf (Keyword "flagship", s, _)): (PNode Block xs): []))			= FuncNode 	s [] (map convert xs)
convert (PNode _ ((PNode _ [PLeaf (Keyword "booty", "booty", _)]): x:[]))				= FuncValNode (convert x) (VarNode "String" 0)
convert (PNode _ ((PNode _ [PLeaf (Keyword "doubloon", "doubloon", _)]): x:[]))			= FuncValNode (convert x) (VarNode "Int" 0)
convert (PNode _ ((PNode _ [PLeaf (Keyword "bool", "bool", _)]): x:[]))					= FuncValNode (convert x) (VarNode "Bool" 0)
convert (PNode _ ((PNode _ [PLeaf (Keyword "treasure", "treasure", _)]): x:[]))			= FuncValNode (convert x) (VarNode "Array" 0)
convert (PNode _ ((PLeaf (Keyword "parrot", s, _)): x: []))								= PrintNode (convert x)
convert (PNode _ ((PLeaf (Keyword "avast", s, _)): x: []))								= ReturnNode s (convert x)
convert (PNode Func ((PLeaf (Idf, s, _)): xs))											= DoFuncNode s (map convert xs)
convert (PNode _ (x: x': []))															= BootyNode (convert x) (convert x')

data Tree = VarNode 	String Int
		  | BootyNode 	Tree Tree
		  | DoubloonNode Tree Tree
		  | BoolNode 	Tree Tree
		  | TreasureNode Tree Tree
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
toRTree (IfNode t1 list)			= RoseNode "if" (map toRTree (t1:list))
toRTree (ElseNode list)				= RoseNode "else" (map toRTree (list))
toRTree (ForNode t1 t2 t3 list)		= RoseNode "for" (map toRTree (t1:t2:t3:list))
toRTree (WhileNode t1 list)			= RoseNode "while" (map toRTree (t1:list))
toRTree (FuncNode s list1 list2)	= RoseNode s (map toRTree (list1 ++ list2))
toRTree (FuncValNode t1 t2)			= RoseNode "param" [toRTree t1, toRTree t2]
toRTree (PrintNode t1)				= RoseNode "print" [toRTree t1]
toRTree (ReturnNode s t1)			= RoseNode s [toRTree t1]
toRTree (DoFuncNode s list)			= RoseNode s (map toRTree list)
toRTree (ZupaNode s list)			= RoseNode s (map toRTree list)

showConvertedTree = showRoseTree $ toRTree $ convert test0	
