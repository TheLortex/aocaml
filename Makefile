
all:
	@dune exec --profile release -- ./solutions/2022/main.exe

last:
	@dune exec -- ./solutions/2022/main.exe --last

stdin:
	@dune exec -- ./solutions/2022/main.exe --last --stdin

fmt:
	dune build @fmt
	dune promote
