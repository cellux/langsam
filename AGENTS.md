# AGENTS.md

This file helps agents and contributors get oriented quickly in this repo.

**Project Overview**
- Langsam is a small Lisp-like language implemented in C with a minimal standard library written in Langsam itself.

**Key Entry Points**
- `langsam.c` holds the VM, core types, reader, evaluator, GC, and most native functionality.
- `langsam.h` exposes the public API, core types, and VM utilities.
- `driver.c` is the CLI entry point for the `langsam` executable.
- `platform/os/linux/os.c` contains Linux-specific `os` module functionality (file IO + module symbol loading).
- `modules/langsam.l` is the core standard library written in Langsam and embedded into the binary.

**Build and Test**
- Build: `make`
- Test suite: `make test`
- Valgrind: `make vtest`
- Debugging: `make gdb`

**Tests**
- Tests are plain Langsam scripts in `tests/*.l`.
- The test runner is `./langsam tests/*.l` as wired in `Makefile`.

**Modules**
- Native modules are discovered from:
  - `modules/*.c`
  - `platform/os/$(LANGSAM_OS)/*.c`
  - `platform/arch/$(LANGSAM_ARCH)/*.c`
- In the current tree, the `os` module lives at `platform/os/linux/os.c`.
- Langsam-side module code lives in `modules/*.l`.
- Modules are resolved at runtime by `langsam_require` via registered module loaders.
- The Linux `os` module registers `os/loadmodule`, which looks up exported `langsam_module_*` symbols.

**Embedding Langsam Code**
- `bin2c.py` converts `.l` files to `.lc` C blobs.
- `Makefile` builds `*.lc` blobs from `modules/*.l` (including `modules/langsam.l`), compiles them to `.lo`, then links them into the binary.

**Conventions and Notes**
- C is compiled with strict warnings and diagnostics (`-Wall -Wextra -Wpedantic`, plus `-Wconversion`, `-Wshadow`, `-Wcast-qual`, `-Wstrict-prototypes`, `-Wmissing-prototypes`, `-Wformat=2`, `-Wsign-conversion`, `-Wundef`, `-Wpointer-arith`).
- Exceptions are first-class `LV` values; most API calls return `LV` and use `LANGSAM_CHECK`.
- The REPL is enabled when no script args are provided.

**Semantics Reference**
- Language behavior and edge cases are documented in `docs/semantics.md`.
