import Sprockell.System

prog = [
       -- doubloon a be 1
       Const 1 RegA,
       Store RegA (Addr 1),
       -- parley(a be 1)
       Const 1 RegA,
       Load (Addr 1) RegB,
       Compute NEq RegB RegA RegA
       Branch RegA Rel(19)
       -- bool b be 1
       Const 1 RegA,
       Store RegA (Addr 2),
       -- parley(b be b)
       Load (Addr 2) RegA,
       Load (Addr 2) RegB,
       Compute NEq RegB RegA RegA
       Branch RegA Rel(11)
       -- doubloon c be 5
       Const 5 RegA,
       Store RegA (Addr 3),
       -- doubloon d be a + c
       Load (Addr 1) RegA,
       Load (Addr 3) RegB,
       Compute Add RegA RegB RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 4),
       -- END
       EndProg
       ]

main = run 1 prog