import Parse

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
endmark = Symbol "!"
colon   = Symbol ":"
star	= Symbol "*"

data State = START | ERROR | KW | SY | NUM | IDF | BOOL | comment

tokenizer :: State -> String -> [Token]
tokenizer _ [] word = []
tokenizer ERROR _ _ = error "Shiver me timbers! You done it wrong."
tokenizer START (x:xs) word | length possibleKeywords > 1 = tokenizer KW xs newWord
							| length possibleKeywords == 1 && startsWith ' ' xs = (KeyWord newWord, newWord): tokenizer START (tail xs) ""
							| length possibleKeywords == 0 = tokenizer ERROR xs word
							| otherwise = tokenizer ERROR xs word
							where 
								possibleKeywords = checkKeywords newWorld
								newWord = (x:word)

checkKeywords ::  String -> [String]
checkKeywords possibleToken = [x | x <- allKeywords, startsWith possibleToken x]

sampleProgram = concat ["fleet Sample {",
						"   booty a be 3!",
						"   booty b be 6!",
						"   booty c be a+b!",
						"   booty d be Aye!",
						"   parlay(d) { *: This is a comment Arr.",
						"      parrot c!",
						"   }",
						"   heave ho {",
						"      parrot Nay!",
						"   }",
						"   whirlpool(d) {",
						"      parlay(c be 10) {",
						"         belay!",
						"      }",
						"      c be c + 1!",
						"   }",
						"}"						
						]

helloWorld = concat ["fleet HelloWorld {",
					 "   parrot \"Ahoy World!\" !",
					 "}"
					]

sampleFunction = concat ["fleet SampleFunction {",
						 "   ship add3(booty i) {",
						 "      avast i + 3!",
						 "   }",
						 "   ",
						 "   flagship() {",
						 "      booty a be add3(5)!",
						 "      parrot a!",
						 "   }",
						 "}"
						]