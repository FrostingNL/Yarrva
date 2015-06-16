import Parse
import Data.Char
import Data.List
import Debug.Trace

{-
TO-DO:
	- Tokenizer
	- Testing (/w GUI)
	- Do the rest
-}

grammar :: Grammar
grammar nt = case nt of
	Program -> [[progKey, lcbr, Rep0 [Expr], rcbr]]
	Expr 	-> [[varKey, idf, equalsKey, num, endmark],
				[printKey, num, endmark]]

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
star	= Symbol "*"

data State = START | ERROR | KW | KWWORD

tokenizer :: State -> String -> [Token]
tokenizer _ [] = []
tokenizer ERROR _ = error "Shiver me timbers! You done it wrong, Arrr!"
tokenizer s (' ':xs) = tokenizer s xs
tokenizer START (x:xs) | ord x >= 97 && ord x <= 122 = tokenizer KW (x:xs)
					   | otherwise = tokenizer ERROR (x:xs)
tokenizer KW (x:xs)
	| isKeyword (x:restWord) 	= (Keyword (x:restWord), x:restWord): tokenizer KW restString
	| otherwise				= (Keyword "var", (x:restWord)): tokenizer KW restString
	where
		restWord = getWord xs
		restString = getRest xs

isKeyword :: String -> Bool
isKeyword s = elem s allKeywords

getWord :: String -> String
getWord [] = []
getWord (x:xs)
	| x == ' '  = ""
	| otherwise = x: getWord xs

getRest :: String -> String
getRest [] = []
getRest (x:xs)
	| x == ' '  = xs
	| otherwise = getRest xs

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