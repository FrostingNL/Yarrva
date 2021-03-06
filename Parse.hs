{-# LANGUAGE FlexibleInstances #-}

module Parse where


import Debug.Trace
import Data.List
import FPPrac.Trees

doPrint = True

-- Embedded language for alphabet: the first 10 clauses should not be removed, the last three can be replaced by your own.
data Alphabet =	  Symbol     String		-- Token given ("char" specific for this example)
		| Keyword    String		-- A given string, but included in the parsetree
		| SyntCat    Alphabet		-- A given string, but included in the parsetree
		| NoCat		 Alphabet
		| CheckChar  (Char->Bool)	-- Character should have some property (for tokenizer)
		| CheckToken (Token->Bool)	-- Token should have some property (for parser)

		| Alt	[Alphabet] [Alphabet]	-- Try both
		| Try	[Alphabet] [Alphabet]	-- If first doesn't work, try second
		| Opt	[Alphabet]		-- Optional
		| Rep0	[Alphabet]		-- Zero or more repetitions
		| Rep1	[Alphabet]		-- One or more repetitions

		-- A few non-terminals as example; to be filled in for your own language
		| Program
		| Stat
		| Expr
		| Expr2
		| Block
		| BoolExpr
		| Idf
		| Nmbr
		| Bool
		| String
		| ArrayList
		| Type
		| TypeName
		| Op
		| FuncVal
		| Assign
		| Func
		| FValues
		| Var
		| Array
		| ArrayOp
		| ArrayIndex
		| Function
		| PBlock
		| MainFunc
		| FuncBlock
		| GlobalVars
		| VarDecl
		| BoolExpr'
		| ArrayElem

		deriving (Eq,Show)


-- functions for shorthand notation for EBNF constructions
ps <>  qs = Alt  ps qs
ps <<> qs = Try  ps qs
(?>) ps   = Opt  ps
(*>) ps   = Rep0 ps
(+>) ps   = Rep1 ps

-- "Hack"
instance Eq   (Char  -> Bool) where f == g = True	-- These instances are a hack, since otherwise "deriving (Eq,Show)"
instance Eq   (Token -> Bool) where f == g = True	-- 	for Alphabet won't work.
instance Show (Char  -> Bool) where show f = ""		-- Just make sure you never apply (==) or show to function types.
instance Show (Token -> Bool) where show f = ""





-- ==========================================================================================================
-- Example grammar, to illustrate the structure of the definition of the grammar as a function

type Grammar = Alphabet -> [[Alphabet]]


-- shorthand names can be handy, such as:
lBracket  = Symbol "("
rBracket  = Symbol ")"

num	  = SyntCat Nmbr
idf	  = SyntCat Idf

prog	  = Keyword "program"
while	  = Keyword "while"
enz	  = Keyword "enzovoort"


-- ==========================================================================================================
-- Parsing	- Note that a ParseTree contains a lot of syntactic information that is not desirable in an AST.
--		  So you still have to write a function that transforms a ParseTree into an AST.
-- Token	- a 2-tuple of a non-terminal and a string, where the non-terminal
--		  indicates to what syntactic category teh string belongs.

type Token	= (Alphabet,String,Int,Int)

data ParseTree	= PLeaf Token			-- PLeaf: ParseTree-Leaf
                | PNode Alphabet [ParseTree]	-- PNode: ParseTree-Node
                deriving (Eq,Show)

type ParseState = ( Alphabet			-- Non-terminal indicating the present subexpression
                  , [ParseTree]			-- The already produced trees within the present subexpression
                  , [Token]			-- The remaining list of input tokens
                  )


-- =================================================================
-- Parser generator
--
-- format:
-- parserGen gr rule (nt,ts,tokens)	- gr	: is the grammar
--					- rule	: is the rule which is now parsed
--					- nt	: is the Non-Terminal immediately above the sequence of subtrees under development
--					- ts	: is the sequence of trees already found, and which will be joined under nt
--					- tokens: the remaining sequence of tokens still to be parsed (of the form (Alphabet,String)).

parserGen :: Grammar -> [Alphabet] -> ParseState -> [(ParseTree,[Token])]

parserGen gr [] (nt0,ts,tokens) = [(PNode nt0 ts, tokens)]

parserGen _  _  ( _ , _,  []  ) = []

parserGen gr (nt:rule) (nt0,ts,(cat,str, l,c):tokens)
 | doPrint = case nt of
	Symbol str'	->  (if (str==str')
				then traceShow ("(" ++ show l ++ ":" ++ show c ++ ")" ++ " success: " ++ str)
                                     (parserGen gr rule (nt0,ts,tokens))
				else traceShow ("    (" ++ show l ++ ":" ++ show c ++ ")" ++ " expected: " ++ str' ++ " -- found: " ++ str)
				     []
				)

	Keyword str'	->  (if (str==str')
				then traceShow ("(" ++ show l ++ ":" ++ show c ++ ")" ++ " success: " ++ str)
				     (parserGen gr rule (nt0, ts++[PLeaf (cat,str, l,c)], tokens))
				else traceShow ("    (" ++ show l ++ ":" ++ show c ++ ")" ++ " expected: " ++ str' ++ " -- found: " ++ str)
				     []
				)

	SyntCat cat'	->  (if (cat==cat')
				then traceShow ("(" ++ show l ++ ":" ++ show c ++ ")" ++ " success: " ++ show cat ++ " " ++ str)
				     (parserGen gr rule (nt0, ts++[PLeaf (cat,str, l,c)], tokens))
				else traceShow ("    (" ++ show l ++ ":" ++ show c ++ ")" ++ " expected: " ++ show cat' ++ " -- found: " ++ show cat ++ " " ++ str)
				     []
				)


	NoCat cat'	->  (if (cat==cat')
				then traceShow ("(" ++ show l ++ ":" ++ show c ++ ")" ++ " success: " ++ show cat ++ " " ++ str)
				     (parserGen gr rule (nt0, ts, tokens))
				else traceShow ("    (" ++ show l ++ ":" ++ show c ++ ")" ++ " expected: " ++ show cat' ++ " -- found: " ++ show cat ++ " " ++ str)
				     []
				)

	CheckToken p	->  (if (p (cat,str,l,c))
				then traceShow ("(" ++ show l ++ ":" ++ show c ++ ")" ++ " success: " ++ show cat ++ " " ++ str)
				     (parserGen gr rule (nt0, ts++[PLeaf (cat,str, l,c)], tokens))
				else traceShow ("    (" ++ show l ++ ":" ++ show c ++ ")" ++ " expected: some property (...) -- found: " ++ show cat ++ " " ++ str)
				     []
				)

	Alt nts mts	->     parserGen gr (nts++rule) (nt0,ts,(cat,str,l,c):tokens)
			    ++ parserGen gr (mts++rule) (nt0,ts,(cat,str,l,c):tokens)


	Try nts mts	->  (if (parserGen gr nts (nt0,ts,(cat,str,l,c):tokens) /= [])
				then (parserGen gr (nts++rule) (nt0,ts,(cat,str,l,c):tokens))
				else (parserGen gr (mts++rule) (nt0,ts,(cat,str,l,c):tokens))
				)

	Opt  nts	->     parserGen gr (nts++rule) (nt0,ts,(cat,str,l,c):tokens)
			    ++ parserGen gr  rule       (nt0,ts,(cat,str,l,c):tokens)

	Rep0 nts	->     parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str,l,c):tokens)
			    ++ parserGen gr  rule                      (nt0,ts,(cat,str,l,c):tokens)

	Rep1 nts	->     parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str,l,c):tokens)




	_		->    	--trace (show ((cat,str))) $
					[  (t2,tokens2)	| r <- gr nt
					   	, (t1,tokens1) <- parserGen gr r (nt,[],(cat,str,l,c):tokens)
					   	, (t2,tokens2) <- parserGen gr rule (nt0,ts++[t1],tokens1)
					   	]
 | otherwise = case nt of
	Symbol str'	->  (if (str==str')
				then 
                                     (parserGen gr rule (nt0,ts,tokens))
				else 
				     []
				)

	Keyword str'	->  (if (str==str')
				then 
				     (parserGen gr rule (nt0, ts++[PLeaf (cat,str, l,c)], tokens))
				else 
				     []
				)

	SyntCat cat'	->  (if (cat==cat')
				then 
				     (parserGen gr rule (nt0, ts++[PLeaf (cat,str, l,c)], tokens))
				else 
				     []
				)


	NoCat cat'	->  (if (cat==cat')
				then 
				     (parserGen gr rule (nt0, ts, tokens))
				else 
				     []
				)

	CheckToken p	->  (if (p (cat,str,l,c))
				then 
				     (parserGen gr rule (nt0, ts++[PLeaf (cat,str, l,c)], tokens))
				else
				     []
				)

	Alt nts mts	->     parserGen gr (nts++rule) (nt0,ts,(cat,str,l,c):tokens)
			    ++ parserGen gr (mts++rule) (nt0,ts,(cat,str,l,c):tokens)


	Try nts mts	->  (if (parserGen gr nts (nt0,ts,(cat,str,l,c):tokens) /= [])
				then (parserGen gr (nts++rule) (nt0,ts,(cat,str,l,c):tokens))
				else (parserGen gr (mts++rule) (nt0,ts,(cat,str,l,c):tokens))
				)

	Opt  nts	->     parserGen gr (nts++rule) (nt0,ts,(cat,str,l,c):tokens)
			    ++ parserGen gr  rule       (nt0,ts,(cat,str,l,c):tokens)

	Rep0 nts	->     parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str,l,c):tokens)
			    ++ parserGen gr  rule                      (nt0,ts,(cat,str,l,c):tokens)

	Rep1 nts	->     parserGen gr (nts ++ (Rep0 nts : rule)) (nt0,ts,(cat,str,l,c):tokens)




	_		->    	--trace (show ((cat,str))) $
					[  (t2,tokens2)	| r <- gr nt
					   	, (t1,tokens1) <- parserGen gr r (nt,[],(cat,str,l,c):tokens)
					   	, (t2,tokens2) <- parserGen gr rule (nt0,ts++[t1],tokens1)
					   	]


-- ==================================================
-- parse:
--	Uses the parser generator function parseGen to produce only the parsetree (i.e., without rest-string).
--	Assumes a deterministic grammar and returns just the head of the list of all successful parse trees.
--
-- parse gr s tokens:	- gr	: grammar
--			- s	: start symbol
--			- tokens: tokenlist, as result of scanner/tokenizer

parse :: Grammar -> Alphabet -> [Token] -> ParseTree

parse gr s tokens	| ptrees /= []	= head ptrees
			        | otherwise	= error ("Parse: parse error - somewhere :-) - add your own traceShows in parseGen")
	where
	  ptrees = [ t  | r <- gr s
			, (t,rem) <- parserGen gr r (s,[],tokens)
			, rem == []
			]

-- ==================================================
-- Testing


-- For graphical representation, two variants of a toRoseTree function. Define your own to get a good view of the parsetree.
-- First open standard_webpage.html

toRoseTree0, toRoseTree1 :: ParseTree -> RoseTree

toRoseTree0 t = case t of
	PLeaf (c,s, l,col)	-> RoseNode "PLeaf" [RoseNode (show l ++ ": (" ++ show c ++ "," ++ s ++ ")") []]
	PNode nt ts	-> RoseNode "PNode" (RoseNode (show nt) [] : map toRoseTree0 ts)


-- ---
toRoseTree1 t = case t of
	PLeaf (c,s, l,col)	-> RoseNode (show c) [RoseNode (show l ++ ":" ++ show col  ++ ": " ++ s) []]
	PNode nt ts	-> RoseNode (show nt) (map toRoseTree1 ts)



-- ==================================================
-- Clearly, you have to define your own embedded language for constrcuctions in your programming language.
--	This will naturally be a recursive algebraic type, such that it effectively represents the AST.
-- Besides, you'll have to transform a parsetree to a tree of this AST type.
-- Finally, the bck-end of the compiler (code-generation) will be a function of an AST into a list of SprIL instructions.
--
-- Just a hint: use pattern matching on trees