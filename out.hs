import Sprockell.System

prog = [
       -- a(i Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(15)),
       Pop RegA,
       Store RegA (Addr 2),
       -- i be i + 1
       Load (Addr 2) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 2),
       -- avast i
       Pop RegE,
       Load (Addr 2) RegA,
       Push RegA,
       Push RegE,
       Pop RegE,
       Jump (Ind RegE),
       -- b(i Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 4),
       Jump (Rel(47)),
       Pop RegA,
       Store RegA (Addr 2),
       -- parrot (i)
       Load (Addr 2) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- parrot (a(i))
              -- a(i)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Load (Addr 2) RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- i be i + a(i)
       Load (Addr 2) RegA,
              -- a(i)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Load (Addr 2) RegA,
       Push RegA,
       Load (Addr 1) RegA,
       Jump (Ind RegA),
       Pop  RegB,
       Compute Add RegA RegB RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 2),
       -- parrot (i)
       Load (Addr 2) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- avast i
       Pop RegE,
       Load (Addr 2) RegA,
       Push RegA,
       Push RegE,
       Pop RegE,
       Jump (Ind RegE),
       -- flagship()
       -- parrot (b(2))
              -- b(2)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 2 RegA,
       Push RegA,
       Load (Addr 4) RegA,
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