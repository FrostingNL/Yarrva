import Sprockell.System

prog = [
       -- flagship()
       -- doubloon i be 0
       Const (0) RegA,
       Store RegA (Addr 2),
       -- whirlpool(i below 8)
       Compute Add PC Zero RegE,
       Push RegE,
       Const (8) RegA,
       Load (Addr 2) RegB,
       Compute GtE RegB RegA RegA,
       Branch RegA (Rel(14)),
       -- parrot (i)
       Load (Addr 2) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- gift i
       Load (Addr 2) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Store RegA (Addr 2),
       Push RegA,
       Pop RegE,
       Jump (Ind RegE),
       Pop RegE,
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'