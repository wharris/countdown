countdown
=========

Solver for the [countdown numbers round](http://en.wikipedia.org/wiki/Countdown_\(game_show\)#Numbers_round) in Haskell. This was only written as an exercise to help me learn the language. It shouldn't be used as an example of good Haskell or to solve the numbers round in safety-critical applications.

To build:

	ghc countdown --make

Usage:

	./countdown number number number number number number target

`countdown` takes a description of the game as seven integer arguments and prints the best solution it can find. The first six arguments are the numbers on the randomly selected tiles. The last argument is the target.
	
Examples:

	$ ./countdown 25 50 75 100 3 6 952
	(100+3)*75*6/50+25 = 952

	$ ./countdown 75 80 2 3 8 7 812
	(3+7)*(80+2)-8 = 812
