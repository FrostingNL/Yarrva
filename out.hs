import Sprockell.System

prog = [
       -- upNumber(x Int, y Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(38)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 3),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 2),
       -- addFive(c Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 5),
       Jump (Rel(16)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 6),
       -- doubloon temp be c + 5
       Load (Addr 6) RegB,
       Const (5) RegC,
       Compute Add RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 8),
       -- avast temp
       Pop RegE,
       Load (Addr 8) RegA,
       Push RegA,
       Push RegE,
       Pop RegE,
       Jump (Ind RegE),
       -- doubloon result be x + y
       Load (Addr 2) RegB,
       Load (Addr 3) RegC,
       Compute Add RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 10),
       -- avast addFive(result)
       Pop RegE,
              -- addFive(result)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 10 RegA,
       Push RegA,
       Load (Addr 5) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       Push RegA,
       Push RegE,
       Pop RegE,
       Jump (Ind RegE),
       -- flagship()
       -- doubloon x be 3
       Const (3) RegA,
       Store RegA (Addr 2),
       -- doubloon y be 5
       Const (5) RegA,
       Store RegA (Addr 3),
       -- parrot (upNumber(xy))
              -- upNumber(x, y)
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
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'