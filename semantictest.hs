module Semantic where

import Parse
import Grammar
import Checker
import Converter
import FPPrac.Trees
import Sprockell.System
import Debug.Trace
import Control.Exception

type TestSuite = (String, Int, [Instruction], (SystemState -> String))

getRegs sysState spid regs = getRegs' (regbank ((sprs sysState) !! spid)) regs
    where getRegs' rs = map (rs !)

febTestSuite a b = ("FebruaryDaysTest Y: " ++ (show a), 1, febTestProg a, febTest b)
febTestProg a = [  				{- Compiled from feb.yarr -}
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
       -- doubloon y be a
       Const (a) RegA,
       Store RegA (Addr 2),
       -- feb(a)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 2 RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       -- END
       EndProg
       ]

febTest a sysState
		| getRegs sysState 0 [RegA] == [a] = "OK" 
    | otherwise                        = "NOPE"

primeTestSuite a b = ("PrimeNumberTest P: " ++ (show a), 1, primeTestProg a, primeTest b)
primeTestProg a = [ {- Compiled from Tests/prime.yarr -} 
       -- isPrime(num Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(48)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 2),
       -- doubloon temp be 0
       Const (0) RegA,
       Store RegA (Addr 4),
       -- order res be 1
       Const (1) RegA,
       Store RegA (Addr 5),
       -- navigate(doubloon i be 2. i be below num. gift i)
       -- doubloon i be 2
       Const (2) RegA,
       Store RegA (Addr 6),
       Compute Add PC Zero RegE,
       Push RegE,
       Load (Addr 2) RegA,
       Load (Addr 6) RegB,
       Compute Gt RegB RegA RegA,
       Branch RegA (Rel(26)),
       -- temp be num % i
       Load (Addr 2) RegB,
       Load (Addr 6) RegC,
       Compute Mod RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 4),
       -- parley(temp be 0)
       Const (0) RegA,
       Load (Addr 4) RegB,
       Compute NEq RegB RegA RegA,
       Branch RegA (Rel(8)),
       -- parley(i be num)
       Load (Addr 2) RegA,
       Load (Addr 6) RegB,
       Compute NEq RegB RegA RegA,
       Branch RegA (Rel(2)),
       Jump (Rel(3)),
       -- heave
       -- res be 0
       Const (0) RegA,
       Store RegA (Addr 5),
       -- gift i
       Load (Addr 6) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Store RegA (Addr 6),
       Push RegA,
       Pop RegA,
       Pop RegE,
       Jump (Ind RegE),
       Pop RegE,
       -- avast res
       Load (Addr 5) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Pop RegD,
       Jump (Ind RegD),
       -- flagship()
       -- doubloon x be a
       Const (a) RegA,
       Store RegA (Addr 2),
       -- isPrime(x)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 2 RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       -- END
       EndProg
       ]
primeTest a sysState
    | getRegs sysState 0 [RegA] == [a] = "OK" 
    | otherwise                        = "NOPE"

divBy0TestSuite = ("Division By 0 Test", 1,divBy0TestProg, divBy0Test)
divBy0TestProg = [  {- Compiled from Tests/divBy0.yarr -}
       -- flagship()
       -- doubloon y be 10 / 0
       Const (10) RegB,
       Const (0) RegC,
       Compute Div RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 2),
       -- END
       EndProg
       ]
divBy0Test sysState
    | getRegs sysState 0 [RegA] /= [] = "NOPE" 
    | otherwise                        = "OK"

-- Running test logic --
runSuite :: TestSuite -> IO ()
runSuite (name, nSprockells, prog, test) = do
    putStr name >> putStr " (n=" >> putStr (show nSprockells) >> putStr "): "
    run nSprockells prog >>= return . test >>= putStr
    putChar '\n'
    return ()
    
main = do
    runSuite $ febTestSuite 2000 29
    runSuite $ febTestSuite 2001 28
    runSuite $ febTestSuite 2002 28
    runSuite $ febTestSuite 2003 28
    runSuite $ febTestSuite 2004 29
    runSuite $ primeTestSuite 14 0
    runSuite $ primeTestSuite 13 1
    result <- try (runSuite $ divBy0TestSuite) :: IO (Either SomeException ())
    case result of
        Left ex  -> putStrLn $ "Caught exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val
    return ()
