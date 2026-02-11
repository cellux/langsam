# AGENTS.md

This file helps agents and contributors get oriented quickly in this repo.

**Project Overview**
- Langsam is a small Lisp-like language implemented in C with a minimal standard library written in Langsam itself.

**Long-Term Bootstrap Roadmap**
- The long-term goal is a full self-hosting systems stack, built incrementally:
  1. Implement a Forth in assembly language called **Grund**.
  2. Implement **Langsam** in Grund.
  3. Implement **Schell** as a library in Langsam.
  4. Reimplement Langsam in **Schnell** and replace the old version with the new one.
  5. Compile and bootstrap an operating system called **Oben**, written in Schnell, using the latest and fastest Langsam.
- End state: a working, running OS built from scratch.
- Constraint: this entire pipeline is intended to happen in memory during system boot.

The first implementation will be hosted on top of the qemu q35 machine using x86-64 architecture.

**Key Entry Points**
- `langsam.c` holds the VM, core types, reader, evaluator, GC, and most native functionality.
- `langsam.h` exposes the public API, core types, and VM utilities.
- `driver.c` is the CLI entry point for the `langsam` executable.
- `platform/os/linux/os.c` contains Linux-specific `os` module functionality (file IO + module symbol loading).
- `modules/langsam.l` is the core standard library written in Langsam and embedded into the binary.
- `modules/x86_64.l` is the x86-64 assembler.

**Build and Test**
- Build: `make`
- Fast tests: `make fasttest`
- Slow tests: `make slowtest`
- Full test suite: `make test`
- Parallel test jobs: optional `TEST_JOBS` override (defaults to CPU core count), for example `make test TEST_JOBS=8`
- Valgrind: `make vtest`
- Debugging: `make gdb`
- Important: rebuild `langsam` before running tests (for example `make` or `make test`), because Langsam modules are embedded into the binary at link time.

**Tests**
- Tests are plain Langsam scripts in `tests/fast/*.l` and `tests/slow/*.l`.
- The test runners are invoked via `make fasttest`, `make slowtest`, and `make test` (runs both).
- `test.py` is the Python test runner used by Make targets and supports `TEST_JOBS` parallelism.

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
- Stdlib helper names prefixed with `-` are internal/private and not part of the supported public API.
- In Langsam, write function docstrings on their own line in `defn` headers, for example:
  ```lisp
  (defn name
    "Docstring."
    [params]
    ...)
  ```

**Semantics Reference**
- Language behavior and edge cases are documented in `docs/semantics.md`.
