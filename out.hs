import Sprockell.System

prog = [
       -- order doWhile be 1
       Const (1) RegA,
       Store RegA (Addr 1),
       -- countDownTo(x Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 2),
       Jump (Rel(38)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 3),
       -- parley(x be below 9)
       Const (9) RegA,
       Load (Addr 3) RegB,
       Compute Gt RegB RegA RegA,
       Branch RegA (Rel(29)),
       -- whirlpool(doWhile)
       Compute Add PC Zero RegE,
       Push RegE,
       Load (Addr 1) RegA,
       Const 1 RegB,
       Compute Equal RegB RegA RegA,
       Branch RegA (Rel(20)),
       -- parley(x above 9)
       Const (9) RegA,
       Load (Addr 3) RegB,
       Compute LtE RegB RegA RegA,
       Branch RegA (Rel(4)),
       -- doWhile be 0
       Const (0) RegA,
       Store RegA (Addr 1),
       Jump (Rel(12)),
       -- heave
       -- parrot (x)
       Load (Addr 3) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- plunder x
       Load (Addr 3) RegA,
       Const 1 RegB,
       Compute Sub RegA RegB RegA,
       Store RegA (Addr 3),
       Push RegA,
       Pop RegA,
       Pop RegE,
       Jump (Ind RegE),
       Pop RegE,
       Pop RegD,
       Jump (Ind RegD),
       -- flagship()
       -- doubloon x be 3
       Const (3) RegA,
       Store RegA (Addr 3),
       -- countDownTo(x)
       Const 6 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 3 RegA,
       Push RegA,
       Load (Addr 2) RegA,
       Jump (Ind RegA),
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'