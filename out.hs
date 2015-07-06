import Sprockell.System

prog = [
       -- function(a Int, b Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 1),
       Jump (Rel(105)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 3),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 2),
       -- comp(x Int, y Int)
       Const 3 RegA,
       Compute Add PC RegA RegE,
       Store RegE (Addr 5),
       Jump (Rel(21)),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 7),
       Pop RegA,
       Load (Deref RegA) RegB,
       Store RegB (Addr 6),
       -- order temp be 0
       Const (0) RegA,
       Store RegA (Addr 9),
       -- parley(x be y)
       Load (Addr 7) RegA,
       Load (Addr 6) RegB,
       Compute NEq RegB RegA RegA,
       Branch RegA (Rel(3)),
       -- temp be 1
       Const (1) RegA,
       Store RegA (Addr 9),
       -- avast temp
       Load (Addr 9) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Pop RegD,
       Jump (Ind RegD),
       -- order d be 0
       Const (0) RegA,
       Store RegA (Addr 11),
       -- doubloon c be a + b
       Load (Addr 2) RegB,
       Load (Addr 3) RegC,
       Compute Add RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 12),
       -- b be c - b
       Load (Addr 12) RegB,
       Load (Addr 3) RegC,
       Compute Sub RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 3),
       -- d be        -- comp(a, b)
       Const 8 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 2 RegA,
       Push RegA,
       Const 3 RegA,
       Push RegA,
       Load (Addr 5) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       Store RegA (Addr 11),
       -- parley(d)
       Load (Addr 11) RegA,
       Const 1 RegB,
       Compute NEq RegB RegA RegA,
       Branch RegA (Rel(8)),
       -- parrot (1)
       Const (1) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       Jump (Rel(7)),
       -- heave
       -- parrot (0)
       Const (0) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- navigate(AssignNode (VarNode "b" 23 15) (VarNode "0" 23 19). b be below a. gift b)
       -- b be 0
       Const (0) RegA,
       Store RegA (Addr 3),
       Compute Add PC Zero RegE,
       Push RegE,
       Load (Addr 2) RegA,
       Load (Addr 3) RegB,
       Compute Gt RegB RegA RegA,
       Branch RegA (Rel(17)),
       -- parrot (b)
       Load (Addr 3) RegA,
       Const (ord '0') RegB,
       Compute Add RegA RegB RegB,
       Write RegB stdio,
       Read (Addr 0x0),
       Receive RegB,
       -- plunder c
       Load (Addr 12) RegA,
       Const 1 RegB,
       Compute Sub RegA RegB RegA,
       Store RegA (Addr 12),
       -- gift b
       Load (Addr 3) RegA,
       Const 1 RegB,
       Compute Add RegA RegB RegA,
       Store RegA (Addr 3),
       Pop RegE,
       Jump (Ind RegE),
       Pop RegE,
       -- avast c
       Load (Addr 12) RegA,
       Pop RegD,
       Push RegA,
       Push RegD,
       Pop RegD,
       Jump (Ind RegD),
       -- flagship()
       -- doubloon e be 1
       Const (1) RegA,
       Store RegA (Addr 2),
       -- doubloon r be 2
       Const (2) RegA,
       Store RegA (Addr 3),
       -- parrot (function(er))
              -- function(e, r)
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