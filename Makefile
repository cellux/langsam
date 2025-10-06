CFLAGS = -g -O0 -fno-omit-frame-pointer
LDFLAGS = -lm

langsam: driver.o langsam.o langsam_l.o os.o

%.c: %.l

langsam_l.o: langsam_l.c
langsam_l.c: langsam.l bin2c.py
	python3 bin2c.py $< $@ langsam_l

driver.o: driver.c langsam.h
langsam.o: langsam.c langsam.h

os.o: os.c langsam.h

.PHONY: test
test: langsam
	valgrind --leak-check=full --show-leak-kinds=all ./langsam tests/*.l

.PHONY: gdb
gdb: langsam
	gdb -x langsam.gdb --args ./langsam tests/*.l

.PHONY: clean
clean:
	rm -fv *.o langsam_l.c langsam
