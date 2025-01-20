all: sieve sieve.o

sieve: sieve.o
	gcc -ggdb sieve.o -ljemalloc -o sieve

sieve.o: sieve.asm
	fasm sieve.asm sieve.o

clean:
	rm -f sieve sieve.o
