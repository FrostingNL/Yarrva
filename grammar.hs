progKey 	= Keyword "ship"
returnKey 	= Keyword "avast"
equalsKey 	= Keyword "be"
trueKey 	= Keyword "Aye"
falseKey 	= Keyword "Nay"
varKey 		= Keyword "booty"
ifExprKey	= Keyword "parley"
whileKey	= Keyword "while"
elseifKey	= Keyword "else parley"
elseKey 	= Keyword "else"
breakKey	= Keyword "belay"
printKey	= Keyword "parrot"
continueKey = Keyword "God's speed"
whileKey	= Keyword "come about"
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

sampleProgram = concat ["fleet Sample {",
						"	booty a be 3!",
						"	booty b be 6!",
						"	booty c be a+b!",
						"	booty d be Aye!",
						"	parlay(d) { *: This is a comment Arr.",
						"		parrot c!",
						" 	}",
						"	heave ho {",
						"		parrot Nay!",
						"	}",
						"	whirlpool(d) {",
						"		parlay(c be 10) {",
						"			belay!",
						"		}",
						"		c be c + 1!",
						"	}",
						"}"						
						]

helloWorld = concat ["fleet HelloWorld {",
					 "	parrot \"Ahoy World!\" !",
					 "}"
					]
