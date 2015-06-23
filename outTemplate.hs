import Sprockell.System

prog :: [Instruction]
prog = [Const 1 RegA,
		Const 2 RegB,
		Compute Add RegA RegB RegC,
		Write RegC stdio, -- write uppercase letter
		Read (Addr 0x0),  -- dummy read to ensure that
        Receive RegA,     -- all write request are done
		EndProg
		]

main = run 1 prog >> putChar '\n'