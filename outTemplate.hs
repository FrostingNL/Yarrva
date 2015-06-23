import Sprockell.System

prog :: [Instruction]
prog = [Const 1 RegA, -- doubloon a be 1
		-- parley(a be 1)
		Const 1 RegB,
		Compute NEq RegA RegB RegC,
		Branch RegC (Rel 7),
		-- parrot("Aye")
		Const (ord 'A') RegA,
		Push RegA,
		Const (ord 'y') RegA,
		Push RegA,
		Const (ord 'e') RegA,
		Push RegA,
		Pop RegA,
		Write RegA stdio,
		Pop RegA,
		Write RegA stdio,
		Pop RegA,
		Write RegA stdio,
		-- End
		Read (Addr 0x0),
        Receive RegC,
		EndProg
		]

main = run 1 prog >> putChar '\n'