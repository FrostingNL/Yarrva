import Data.Char
import FPPrac.Trees
import Data.List
import Debug.Trace
import System.IO
import System.Environment
import Parse
import Grammar
import Checker

start :: FilePath -> IO ()
start input = do
	inHandle <- openFile input ReadMode  
	contents <- hGetContents inHandle
	putStrLn ("SDSS: " ++ contents)
	outHandle <- openFile "out.hs" WriteMode
	hPutStr outHandle $ toSprockell $ convert $ parse grammar Program $ tokens contents
	hClose inHandle
	hClose outHandle

toSprockell :: Tree -> String
toSprockell tree = 
	case tree of
		(DoubloonNode t1 t2)	-> "Const " ++ (getValue t2) ++ " RegA, "
		(ZupaNode s xs)			-> "import Sprockell.System\n\nprog = [" ++ (concat (map (toSprockell) xs)) ++ "EndProg]\n\nmain = run 1 prog"
		_						-> ""