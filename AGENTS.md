# AGENTS.md

This file helps agents and contributors get oriented quickly in this repo.

**Project Overview**
- Langsam is a small Lisp-like language implemented in C with a minimal standard library written in Langsam itself.

**Key Entry Points**
- `langsam.c` holds the VM, core types, reader, evaluator, GC, and most native functionality.
- `langsam.h` exposes the public API, core types, and VM utilities.
- `driver.c` is the CLI entry point for the `langsam` executable.
- `driver.l` appears to be unused by the current build (no references in `Makefile` or code).
- `langsam.l` is the core standard library written in Langsam and embedded into the binary.

**Build and Test**
- Build: `make`
- Test suite: `make test`
- Valgrind: `make vtest`
- Debugging: `make gdb`

**Tests**
- Tests are plain Langsam scripts in `tests/*.l`.
- The test runner is `./langsam tests/*.l` as wired in `Makefile`.

**Modules**
- Native modules live in `modules/*.c`.
- `modules/os.c` provides the `os` module with file IO and process arguments.
- Langsam-side module code lives in `modules/*.l`.
- The CLI registers modules in `driver.c` via `langsam_register_module`.

**Embedding Langsam Code**
- `bin2c.py` converts `.l` files to `.lc` C blobs.
- `Makefile` builds `langsam.lc` from `langsam.l`, then compiles and links it into the binary.

**Conventions and Notes**
- C is compiled with strict warnings (`-Wall -Wextra -Wpedantic -Wconversion -Wshadow`).
- Exceptions are first-class `LV` values; most API calls return `LV` and use `LANGSAM_CHECK`.
- The REPL is enabled when no script args are provided.
