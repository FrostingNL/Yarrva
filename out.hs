import Sprockell.System

prog = [
       -- a(b Int, c Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(20)),
       Pop RegA,
       Store RegA (Addr 3),
       Pop RegA,
       Store RegA (Addr 2),
       -- parrot (b)
       Load (Addr 2) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- parrot (c)
       Load (Addr 3) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       Pop RegE,
       Push RegA,
       Jump (Ind RegE),
       -- a(1, 2)
       Const 8 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 1 RegA,
       Push RegA,
       Const 2 RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop RegA,
       -- a(3, 4)
       Const 8 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 3 RegA,
       Push RegA,
       Const 4 RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop RegA,
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'