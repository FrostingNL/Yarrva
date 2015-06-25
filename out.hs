import Sprockell.System

prog = [
       -- navigate(doubloon i be 0. i be below 5. gift i)
       -- doubloon i be 0
       Const 0 RegA,
       Store RegA (Addr 1),
       Compute Add PC Zero RegE,
       Push RegE,
       Const 5 RegA,
       Load (Addr 1) RegB,
       Compute Gt RegB RegA RegA,
       Branch RegA (Rel(28)),
       -- doubloon a be 0
       Const 0 RegA,
       Store RegA (Addr 3),
       -- whirlpool(a be below 2)
       Compute Add PC Zero RegE,
       Push RegE,
       Const 2 RegA,
       Load (Addr 3) RegB,
       Compute Gt RegB RegA RegA,
       Branch RegA (Rel(13)),
       -- gift a
       Load (Addr 3) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Store RegA (Addr 3),
       -- parrot (a)
       Load (Addr 3) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       Pop RegE,
       Jump (Ind RegE),
       Pop RegE,
       -- gift i
       Load (Addr 1) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Store RegA (Addr 1),
       Pop RegE,
       Jump (Ind RegE),
       Pop RegE,
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'