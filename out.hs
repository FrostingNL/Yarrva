import Sprockell.System

prog = [
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
       -- avast 28
       Pop RegE,
       Const (28) RegA,
       Push RegA,
       Push RegE,
       Jump (Rel(5)),
       -- heave
       -- avast 29
       Pop RegE,
       Const (29) RegA,
       Push RegA,
       Push RegE,
       Pop RegE,
       Jump (Ind RegE),
       -- flagship()
       -- doubloon year be 2001
       Const (2001) RegA,
       Store RegA (Addr 2),
       -- parrot (feb(year))
              -- feb(year)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 2 RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'