import Parse
import Data.Char
import Data.List
import Debug.Trace
import FPPrac.Trees

grammar :: Grammar
grammar nt = case nt of
	Program -> [[progKey, idf, lcbr, Rep0 [Stat], rcbr]]
	Stat 	-> [[varKey, idf, equalsKey, Expr, endmark]]
	Expr 	-> [[Type, SyntCat Op, Type],
				[Type]]
	Op		-> [[plus],
				[minus],
				[times],
				[divide],
				[notSym]]
	Type	-> [[Nmbr],
				[Bool],
				[idf]]
	Nmbr 	-> [[SyntCat Nmbr]]
	Bool 	-> [[SyntCat Bool]]

progKey 	= Keyword "fleet"
functionKey = Keyword "ship"
returnKey 	= Keyword "avast"
equalsKey 	= Keyword "be"		-- n be a
lesserKey	= Keyword "lower"	-- n be lower a
greaterKey	= Keyword "higher"	-- n be higher a
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

data State = START | ERROR | KW | KWWORD

tokenizer :: State -> String -> [Token]
tokenizer _ [] = []
tokenizer s (' ':xs) = tokenizer s xs

tokenizer ERROR _ = error "Shiver me timbers! You done it wrong, Arrr!"

tokenizer START (x:xs) | ord x >= 97 && ord x <= 122 = tokenizer KW (x:xs)
					   | otherwise = tokenizer ERROR (x:xs)

tokenizer KW ('{':xs) = (lcbr, ['{']): tokenizer KW xs
tokenizer KW ('}':xs) = (rcbr, ['}']): tokenizer KW xs
tokenizer KW (x:xs) 
	| startsWith getEndmark (x:xs)					= (endmark, x:xs): tokenizer KW (rmEndMark (x:xs))
	| isBoolean (x:restWord)						= (Bool, x:restWord) : tokenizer KW restString
	| isKeyword (x:restWord) 						= (Keyword (x:restWord), x:restWord): tokenizer KW restString
	| elem x "+-*/"									= (Op, [x]): tokenizer KW (restWord ++ restString) 
	| isNumber x									= (Nmbr, x:restNumber): tokenizer KW restString
	| otherwise										= (Idf, (x:restWord)): tokenizer KW restString
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
	| x == ' ' = []
	| otherwise = x: getWord xs

getRest :: String -> String
getRest [] = []
getRest (x:xs)
	| x == ',' = (x:xs)
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
			   "    booty chest be Aye, Arrr!",
			   "    booty b be 2 + 3, Arrr!",
			   "    booty c be a + b, Arrr!",
			   "}"
			   ]

tokens = tokenizer START

test0 = parse grammar Program $ tokens test

showTestTree = showRoseTree $ toRoseTree1 test0

printTupList :: [(Alphabet, String)] -> IO String
printTupList [t] = do return (show t)
printTupList (t:tup) = do
						putStrLn (show t)
						printTupList tup