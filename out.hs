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
       -- subtract(x Int, y Int)
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
       -- doubloon c be        -- add(a, 5)
       Const 8 RegA,
       Compute Add PC RegA RegE,
       Push RegE,
       Const 1 RegA,
       Push RegA,
       Const 0 RegA,
       Push RegA,
       Load (Addr 3) RegA,
       Jump (Ind RegA),
       Pop  RegA,
       Store RegA (Addr 2),
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'