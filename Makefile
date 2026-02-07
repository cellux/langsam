.DEFAULT_GOAL := langsam

CFLAGS += -std=c17 -D_GNU_SOURCE -D_POSIX_C_SOURCE=200809L
CFLAGS += -g -O0 -fno-omit-frame-pointer
CFLAGS += -Wall -Wextra -Wpedantic
CFLAGS += -Wno-unused-parameter
CFLAGS += -Wshadow -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes
CFLAGS += -Wformat=2 -Wconversion -Wsign-conversion -Wundef -Wpointer-arith

LDFLAGS += -rdynamic -Wl,--export-dynamic -ldl -lm

probe: probe.c
	$(CC) -o $@ $<

config.mk: probe
	./probe > config.mk

include config.mk

$(info LANGSAM_OS=$(LANGSAM_OS))
$(info LANGSAM_ARCH=$(LANGSAM_ARCH))

MODULE_C_SRCS := $(shell find modules -name '*.c')
MODULE_L_SRCS := $(shell find modules -name '*.l')

MODULE_C_OBJS := $(MODULE_C_SRCS:.c=.o)
MODULE_L_OBJS := $(MODULE_L_SRCS:.l=.lo)

CORE_OBJS := langsam.o driver.o

langsam: $(CORE_OBJS) $(MODULE_C_OBJS) $(MODULE_L_OBJS)
	$(CC) -o $@ $(CORE_OBJS) $(MODULE_C_OBJS) $(MODULE_L_OBJS) $(LDFLAGS)

$(CORE_OBJS): langsam.h
$(MODULE_C_OBJS): langsam.h

# disable Make builtin which would process *.l with lex
%.c: %.l

modules/%.lc: modules/%.l bin2c.py
	python3 bin2c.py $< $@ langsam_module_$(subst /,_,$*)

%.lo: %.lc
	$(CC) -c -o $@ -x c $<

.PHONY: test
test: langsam
	./langsam tests/*.l

.PHONY: vtest
vtest: langsam
	valgrind --leak-check=full --show-leak-kinds=all ./langsam tests/*.l

.PHONY: gdb
gdb: langsam
	gdb -x langsam.gdb --args ./langsam tests/*.l

.PHONY: clean
clean:
	find -type f -name '*.a' -print -delete
	find -type f -name '*.o' -print -delete
	find -type f -name '*.lo' -print -delete
	find -type f -name '*.lc' -print -delete
	rm -fv langsam probe
