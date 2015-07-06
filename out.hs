import Sprockell.System

prog = [
       -- flagship()
       -- doubloon y be 10 / 0
       Const (10) RegB,
       Const (0) RegC,
       Compute Div RegB RegC RegA,
       Push RegA,
       Pop  RegA,
       Store RegA (Addr 2),
       -- END
       EndProg
       ]

main = run 1 prog >> putChar '\n'