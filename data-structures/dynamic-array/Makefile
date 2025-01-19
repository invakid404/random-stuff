all: main dynamic_array.o

main: main.c dynamic_array.o
	gcc -ggdb main.c dynamic_array.o -ljemalloc -o main

dynamic_array.o: dynamic_array.asm
	fasm dynamic_array.asm dynamic_array.o

clean:
	rm -f dynamic_array dynamic_array.o
