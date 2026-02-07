# Import implementation

## Link model

All modules found/defined by the `Makefile` module lists must be
force-linked into the final executable so their symbols are always
available for runtime lookup.

The executable link flags must include:

- `-rdynamic` / `--export-dynamic` so module symbols are visible to `dlsym`
- `-ldl` for dynamic symbol lookup support

## Module symbol naming

In Langsam core, all imports are implemented by:

```c
LV langsam_require(LangsamVM *vm, char *module_name);
```

Module symbols are derived from the Langsam module name through:

```c
char *langsam_mangle(char *name);
```

Mangling rule: every character that is not valid as part of a C
identifier is replaced with `_`.

For a module named `MODNAME`, where `MANGLED = langsam_mangle(MODNAME)`,
the runtime looks up the following symbols via `dlsym`:

- `langsam_module_MANGLED_size`: size of module Langsam source in bytes
- `langsam_module_MANGLED_data`: module Langsam source bytes
- `langsam_module_MANGLED_load`: `LangsamImportFn` that imports native bindings

## Module cache

Loaded modules are kept in the top-level rootlet variable `modules`
(a `Map`). Keys are module names (`String`), values are module namespaces
(`Map`).

## `langsam_require` load order

If `langsam_require` does not find `module_name` in `modules`, it:

1. Creates a new empty module namespace whose prototype is `vm->rootlet`.
2. Immediately stores that namespace in `modules` under `module_name`
   (prevents cyclic imports from recursing forever).
3. If `langsam_module_MANGLED_load` exists, calls it as `LangsamImportFn`
   with the new module namespace as `env`.
4. If both `langsam_module_MANGLED_size` and
   `langsam_module_MANGLED_data` exist, loads them with
   `langsam_loadstringn` into the same namespace.
5. Returns the module namespace.

If the module is already present in `modules`, `langsam_require` returns it
as-is.

## Error handling

Any failure in module resolution or loading must return an exception with:

- symbol: `require`
- message: explanatory text describing the failure

This includes missing module symbols, malformed symbol combinations
(for example `size` without `data`), native import errors, and source load
errors.
