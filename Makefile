.DEFAULT_GOAL := langsam

BASE_CFLAGS := -std=c17 -D_GNU_SOURCE -D_POSIX_C_SOURCE=200809L
WARN_CFLAGS := -Wall -Wextra -Wpedantic -Wno-unused-parameter
WARN_CFLAGS += -Wshadow -Wcast-qual -Wstrict-prototypes -Wmissing-prototypes
WARN_CFLAGS += -Wformat=2 -Wconversion -Wsign-conversion -Wundef -Wpointer-arith

DEBUG_CFLAGS := -O0 -g -fno-omit-frame-pointer
RELEASE_CFLAGS := -O2
PROFILE_CFLAGS := -O2 -g -fno-omit-frame-pointer

CFLAGS ?= $(BASE_CFLAGS) $(DEBUG_CFLAGS) $(WARN_CFLAGS)

LDFLAGS += -rdynamic -Wl,--export-dynamic -ldl -lm

probe: probe.c
	$(CC) -o $@ $<

config.mk: probe
	./probe > config.mk

include config.mk

$(info LANGSAM_OS=$(LANGSAM_OS))
$(info LANGSAM_ARCH=$(LANGSAM_ARCH))

MODULE_C_SRCS := \
	$(wildcard modules/*.c) \
	$(wildcard platform/os/$(LANGSAM_OS)/*.c) \
	$(wildcard platform/arch/$(LANGSAM_ARCH)/*.c)

MODULE_L_SRCS := \
	$(wildcard modules/*.l) \
	$(wildcard platform/os/$(LANGSAM_OS)/*.l) \
	$(wildcard platform/arch/$(LANGSAM_ARCH)/*.l)

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
	python3 bin2c.py $< $@ langsam_module_$(basename $(notdir $<))

%.lo: %.lc
	$(CC) -c -o $@ -x c $<

.PHONY: test
test: langsam
	./langsam tests/*.l

.PHONY: bench
bench: langsam
	@echo "bench/perf_smoke.l x10"
	@bash -lc 'time -p for i in $$(seq 1 10); do ./langsam bench/perf_smoke.l >/dev/null; done'
	@echo "tests/*.l x20"
	@bash -lc 'time -p for i in $$(seq 1 20); do ./langsam tests/*.l >/dev/null; done'

.PHONY: vtest
vtest: langsam
	valgrind --leak-check=full --show-leak-kinds=all ./langsam tests/*.l

.PHONY: gdb
gdb: langsam
	gdb -x langsam.gdb --args ./langsam tests/*.l

.PHONY: debug
debug:
	$(MAKE) clean
	$(MAKE) CFLAGS='$(BASE_CFLAGS) $(DEBUG_CFLAGS) $(WARN_CFLAGS)'

.PHONY: release
release:
	$(MAKE) clean
	$(MAKE) CFLAGS='$(BASE_CFLAGS) $(RELEASE_CFLAGS) $(WARN_CFLAGS)'

.PHONY: profile
profile:
	$(MAKE) clean
	$(MAKE) CFLAGS='$(BASE_CFLAGS) $(PROFILE_CFLAGS) $(WARN_CFLAGS)'

.PHONY: clean
clean:
	find -type f -name '*.a' -print -delete
	find -type f -name '*.o' -print -delete
	find -type f -name '*.lo' -print -delete
	find -type f -name '*.lc' -print -delete
	rm -fv langsam probe
