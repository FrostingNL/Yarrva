Files needed to compile
	- Parse.hs
	- grammar.hs
	- checker.hs
	- converter.hs
To execute the compiled program:
	- The generated out.hs

To compile:
	. Create a file (preferably with a .yarr extension, but this is not neccesairy)
	. Run the start function in converter.hs with a file path (i.e. start "Example Programs/test,yarr")
	. Exit converter.hs
	. Run the main function of out.hs