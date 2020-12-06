
all:
	@dune exec --profile release -- ./solutions/main.exe

last: 
	@dune exec -- ./solutions/main.exe --last

stdin:
	@dune exec -- ./solutions/main.exe --last --stdin

fmt: 
	dune build @fmt
	dune promote
