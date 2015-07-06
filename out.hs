import Sprockell.System

prog = [
       -- isPrime(num Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(33)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 2),
       -- navigate(doubloon i be 2. i be below num. gift i)
       -- doubloon i be 2
       Const (2) RegA,
       Store RegA (Addr 4),
       Compute Add PC Zero RegE,
       Push RegE,
       Load (Addr 2) RegA,
       Load (Addr 4) RegB,
       Compute Gt RegB RegA RegA,
       Branch RegA (Rel(17)),
       -- parrot (i)
       Load (Addr 4) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- gift i
       Load (Addr 4) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Store RegA (Addr 4),
       Push RegA,
       Pop RegA,
       Pop RegE,
       Push RegA,
       Jump (Ind RegE),
       Pop RegE,
       -- avast 1
       Const (1) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Pop RegD,
       Jump (Ind RegD),
       -- flagship()
       -- doubloon x be 13
       Const (13) RegA,
       Store RegA (Addr 2),
       -- parrot (isPrime(x))
              -- isPrime(x)
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