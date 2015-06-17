import Parse
import Data.Char
import Data.List
import Debug.Trace
import FPPrac.Trees
import System.IO

grammar :: Grammar
grammar nt = case nt of 																			-- The Grammar sorted by occurence
	Program -> [[progKey, idf, Block]]																			-- The Main Program
	Stat 	-> [[Opt [varKey], idf, NoCat equalsKey, Expr, NoCat endmark],													-- Var declaration
				[ifExprKey, lpar, BoolExpr, rpar, Block, Opt[elseKey, Block]],									-- If Expression
				[forKey, lpar, Assign, point, BoolExpr, point, Expr, rpar, Block],	-- For Expression
				[whileKey, lpar, BoolExpr, rpar, Block],														-- While Expression
				[returnKey, Opt [Expr], endmark],																-- Return Expression
				[functionKey, idf, lpar, Opt [FuncVal, Rep0 [comma, FuncVal]], rpar, Block],					-- Normal Function
				[mainKey, lpar, rpar, Block]]																	-- Main Function
	Block	-> [[lcbr, Rep0 [Stat], rcbr]]																		-- A block of code
	BoolExpr-> [[Expr, Alt [Alt [equalsKey] [lesserKey]] [greaterKey], Expr],									-- A boolean expression
				[Bool],																							-- A boolean
				[idf],																							-- An identifier
				[BoolExpr, Alt [orKey] [andKey], BoolExpr]] 													-- Two boolean expressions
	Expr 	-> [[Type, SyntCat Op, Type],																		-- An expression
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
				[idf]]																							-- An identifier
	TypeName-> [[Keyword "Doubloon"],																			-- The name of the Type Number
				[Keyword "Bool"]]																				-- The name of the Type Boolean
	FuncVal	-> [[varKey, idf, equalsKey, TypeName]]																-- The variable you can use in a function decleration
	Assign 	-> [[Opt [varKey], idf, NoCat equalsKey, Expr]]

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
elseifKey	= Keyword "heave to"
elseKey 	= Keyword "heave ho"
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
allKeywords = ["fleet", "ship", "avast", "be", "lower", "higher", "Aye", "Nay", "booty", "parley", "heave to", "heave ho", "belay", "parrot", "God's speed", "whirlpool", "navigate"]

startsWith :: String -> String -> Bool
startsWith [] _ 	= True
startsWith _ [] 	= False
startsWith (s:search) (w:word) 
	| s == w = startsWith search word
	| otherwise = False

sampleProgram = concat ["fleet Sample {",
						"   booty a be 3, Arrr!",
						"   booty b be 6, Arrr!",
						"   booty c be a+b, Arrr!",
						"   booty d be Aye, Arrr!",
						"   parlay(d) { *: This is a comment.",
						"      parrot c, Arrr!",
						"   }",
						"   heave ho {",
						"      parrot Nay, Arrr!",
						"   }",
						"   whirlpool(d) {",
						"      parlay(c be 10) {",
						"         belay, Arrr!",
						"      }",
						"      c be c + 1, Arrr!",
						"   }",
						"}"						
						]

helloWorld = concat ["fleet HelloWorld {",
					 "   parrot \"Ahoy World!\", Arrr!",
					 "}"
					]

sampleFunction = concat ["fleet SampleFunction {",
						 "   ship add3(booty i) {",
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

test2 = concat ["fleet Prog {",
				"   ship Func(booty n be Int, booty b be Bool) {",
				"       booty a be 2, Arrr!",
				"       avast a, Arrr!",
				"   }",
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
	showRoseTree $ toRoseTree1 $ parse grammar Program $ tokens contents

convert :: ParseTree -> Tree
convert (PLeaf (a, s)) 
	| elem (show a) ["Idf", "Bool", "Nmbr"] 						= TypeNode (show a) s
	| otherwise														= ZupaNode []

convert (PNode _ ((PLeaf (Keyword "booty", "booty")):x:x':[]))		= BootyNode (convert x) (convert x')
convert (PNode _ ((x: (PLeaf (Op,s)): x': [])))						= OpNode s (convert x) (convert x')
convert (PNode _ [PLeaf (a, s)])									= TypeNode (show a) s
convert (PNode _ [node])											= convert node
convert (PNode _ ((PLeaf (Keyword "parley", s)):x:xs))			= IfNode s (convert x) (map convert xs)

convert (PNode _ list) = (map convert list)!!2

data Tree = TypeNode 	String String
		  | BootyNode 	Tree Tree
		  | OpNode 		String Tree Tree
		  | BoolExNode 	BoolEx
		  | GiftNode	Tree
		  | PlunderNode Tree
		  | IfNode		String Tree [Tree]
		  | ForNode		String Tree Tree Tree [Tree]
		  | WhileNode	String Tree [Tree]
		  | ZupaNode	[Tree]
		  deriving (Eq, Show)

data BoolEx = String Tree Tree
			| Tree
			deriving (Eq, Show)