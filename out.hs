import Sprockell.System

prog = [
       -- addDiffNumbers(x Int, y Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(28)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 3),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 2),
       -- parley(x be y)
       Load (Addr 3) RegA,
       Load (Addr 2) RegB,
       Compute NEq RegB RegA RegA,
       Branch RegA (Rel(6)),
       -- avast 1
       Const (1) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Jump (Rel(11)),
       -- heave
       -- doubloon sum be x + y
       Load (Addr 2) RegB,
       Load (Addr 3) RegC,
       Compute Add RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 5),
       -- avast sum
       Load (Addr 5) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Pop RegD,
       Jump (Ind RegD),
       -- flagship()
       -- doubloon x be 7
       Const (7) RegA,
       Store RegA (Addr 2),
       -- doubloon y be 1
       Const (1) RegA,
       Store RegA (Addr 3),
       -- parrot (addDiffNumbers(xy))
              -- addDiffNumbers(x, y)
       Const 8 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 2 RegA,
       Push RegA,
       Const 3 RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- doubloon a be 2
       Const (2) RegA,
       Store RegA (Addr 3),
       -- doubloon b be 2
       Const (2) RegA,
       Store RegA (Addr 4),
       -- parrot (addDiffNumbers(ab))
              -- addDiffNumbers(a, b)
       Const 8 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 3 RegA,
       Push RegA,
       Const 4 RegA,
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