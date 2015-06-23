import Sprockell.System

prog = [
       -- doubloon a be 1
       Const 1 RegA,
       Store RegA (Addr 1),
       -- parley(a be 1)
       Const 1 RegA,
       Load (Addr 1) RegB,
       Compute NEq RegB RegA RegA
       Branch RegA Rel(8)
       -- bool b be Aye
       Const 1 RegA,
       Store RegA (Addr 2),
       -- parley(b be Aye)
       Const Aye RegA,
       Load (Addr 2) RegB,
       Compute NEq RegB RegA RegA
       Branch RegA Rel(3)
       -- doubloon c be 5
       Const 5 RegA,
       Store RegA (Addr 3),
       EndProg
       ]

main = run 1 prog