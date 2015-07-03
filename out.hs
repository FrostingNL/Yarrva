import Sprockell.System

prog = [
       -- doubloon a be 1
       Const (1) RegA,
       Store RegA (Addr 1),
       -- order b be 1
       Const (1) RegA,
       Store RegA (Addr 2),
       -- add(x Int, y Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 3),
       Jump (Rel(19)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 5),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 4),
       -- doubloon temp be x + y
       Load (Addr 4) RegB,
       Load (Addr 5) RegC,
       Compute Add RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 7),
       -- avast temp
       Pop RegE,
       Load (Addr 7) RegA,
       Push RegA,
       Push RegE,
       Pop RegE,
       Jump (Ind RegE),
       -- sub(x Int, y Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 9),
       Jump (Rel(19)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 5),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 4),
       -- doubloon temp be x - y
       Load (Addr 4) RegB,
       Load (Addr 5) RegC,
       Compute Sub RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 7),
       -- avast temp
       Pop RegE,
       Load (Addr 7) RegA,
       Push RegA,
       Push RegE,
       Pop RegE,
       Jump (Ind RegE),
       -- flagship()
       -- doubloon five be 5
       Const (5) RegA,
       Store RegA (Addr 2),
       -- parley(b)
       Load (Addr 2) RegA,
       Const 1 RegB,
       Compute Equal RegB RegA RegA,
       Branch RegA (Rel(16)),
       -- parrot (add(afive))
              -- add(a, five)
       Const 8 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 1 RegA,
       Push RegA,
       Const 2 RegA,
       Push RegA,
       Load (Addr 3) RegA,
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