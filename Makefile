CFLAGS = -std=c17 -D_GNU_SOURCE -D_POSIX_C_SOURCE=200809L
CFLAGS += -g -O0 -fno-omit-frame-pointer
CFLAGS += -Wall -Wextra -Wpedantic
CFLAGS += -Wno-unused-parameter
CFLAGS += -Wshadow -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes
CFLAGS += -Wformat=2 -Wconversion -Wsign-conversion -Wundef -Wpointer-arith

LDFLAGS = -lm

LIBS := langsam.a
OBJS := os.o driver.o

langsam: $(LIBS) $(OBJS)

$(LIBS:.a=.o): langsam.h
$(OBJS): langsam.h

%.c: %.l

%.lc: %.l bin2c.py
	python3 bin2c.py $< $@ $(basename $<)_l

%.lo: %.lc
	$(CC) -c -o $@ -x c $<

%.a: %.o %.lo
	$(AR) r -o $@ $^

.PHONY: test
test: langsam
	valgrind --leak-check=full --show-leak-kinds=all ./langsam tests/*.l

.PHONY: gdb
gdb: langsam
	gdb -x langsam.gdb --args ./langsam tests/*.l

.PHONY: clean
clean:
	rm -fv *.a *.o *.lo *.lc langsam
