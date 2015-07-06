module Semantic where

import Parse
import Grammar
import Checker
import Converter
import FPPrac.Trees
import Sprockell.System
import Debug.Trace

type TestSuite = (String, Int, [Instruction], (SystemState -> String))

getRegs sysState spid regs = getRegs' (regbank ((sprs sysState) !! spid)) regs
    where getRegs' rs = map (rs !)

febTestSuite = ("FebruaryDaysTest", 1, febTestProg, febTest)
febTestProg = [  				{- Compiled from feb.yarr -}
       -- feb(year Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(25)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 2),
       -- doubloon temp be year % 4
       Load (Addr 2) RegB,
       Const (4) RegC,
       Compute Mod RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 4),
       -- parley(temp be 0)
       Const (0) RegA,
       Load (Addr 4) RegB,
       Compute NEq RegB RegA RegA,
       Branch RegA (Rel(6)),
       -- avast 29
       Const (29) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Jump (Rel(5)),
       -- heave
       -- avast 28
       Const (28) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Pop RegD,
       Jump (Ind RegD),
       -- flagship()
       -- doubloon y be 2032
       Const (2032) RegA,
       Store RegA (Addr 2),
       -- feb(2032)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 0 RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       -- END
       EndProg
       ]

febTest sysState
		| getRegs sysState 0 [RegA] == [29] = "OK" 

-- Running test logic --
runSuite :: TestSuite -> IO ()
runSuite (name, nSprockells, prog, test) = do
    putStr name >> putStr " (n=" >> putStr (show nSprockells) >> putStr "): "
    run nSprockells prog >>= return . test >>= putStr
    putChar '\n'
    return ()
    
main = do
    runSuite febTestSuite
    return ()
