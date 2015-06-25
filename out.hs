import Sprockell.System

prog = [
       -- navigate(doubloon a be 0. a be below 5. gift a)
       -- doubloon a be 0
       Const 0 RegA,
       Store RegA (Addr 1),
       Compute Add PC Zero RegE,
       Const 5 RegA,
       Load (Addr 1) RegB,
       Compute GtE RegB RegA RegA,
       Branch RegA (Rel(6)),
       -- gift a
       Load (Addr 1) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Store RegA (Addr 1),
       Jump (Ind RegE),
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