CC=gcc
CFLAGS=-I.
DEPS = RTS.h
OBJ = RTS.o


%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

ahrt: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS)

clean:
	rm -rf ahrt $(OBJ)
