import Sprockell.System

prog = [
       -- navigate(doubloon a be 0. a be below 5. gift a)
       -- END
       Const (ord '0') RegB,
       Load (Addr 1) RegA,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       EndProg
       ]

main = run 1 prog >> putChar '\n'