# Quote and Quasiquote

This document covers reader sugar and evaluation behavior for quoting forms.

## Reader Sugar

The reader expands these forms:

- `'x` => `(quote x)`
- `` `x `` => `(quasiquote x)`
- `,x` => `(unquote x)`
- `,@x` => `(unquote-splicing x)`

`quote` and `quasiquote` are special forms. `unquote` and `unquote-splicing`
are interpreted by `quasiquote`.

## `quote`

`quote` returns its argument unchanged and does not evaluate it.

```lisp
'foo            ; => foo (Symbol)
'(1 2 3)        ; => (1 2 3)
```

## `quasiquote`

`quasiquote` recursively walks data and only evaluates forms under `unquote`
or `unquote-splicing`.

High-level behavior by input kind:

- `Cons` (lists): recursively process each element.
- `Vector`: recursively process each element, then rebuild a `Vector`.
- `Map`: recursively process map entries, then rebuild a `Map`.
- atoms (`Symbol`, `Keyword`, numbers, strings, etc.): returned as-is.

Examples:

```lisp
`foo                         ; => foo
`(1 2 3)                     ; => (1 2 3)
(let [x 2] `(1 ,x 3))        ; => (1 2 3)
`[1 ,@'(2 3) 4]              ; => [1 2 3 4]
`{:a ,(+ 1 2) :b 4}          ; => {:a 3 :b 4}
```

## `unquote`

`(unquote form)` (`,form`) evaluates `form` and inserts the resulting value.
`unquote` expects exactly one argument.

```lisp
`(1 ,(+ 1 2) 4)              ; => (1 3 4)
```

## `unquote-splicing`

`(unquote-splicing form)` (`,@form`) evaluates `form`, iterates it, and splices
its elements into the surrounding sequence being built.
`unquote-splicing` expects exactly one argument.

```lisp
`(1 ,@'(2 3) 4)              ; => (1 2 3 4)
`[1 ,@'(2 3) 4]              ; => [1 2 3 4]
```

Rules:

- The spliced value must be iterable.
- As a top-level quasiquote target, `unquote-splicing` is invalid and raises
  with payload like `(%splice ...)`.

## Dotted Lists

For improper lists, the non-list tail is preserved as the final cdr.

```lisp
`(a b . "c")                 ; => (a b . "c")
```

## Nested Quasiquote

Runtime `quasiquote` does not track quasiquote nesting depth as a separate mode.
It recursively processes nested structures, so `unquote` forms inside nested
`quasiquote` forms are still handled during evaluation.

```lisp
(quasiquote (a (quasiquote (b (unquote (+ 1 2))))))
; => (a (quasiquote (b 3)))
```

If you need nesting-aware transformation behavior, that is handled separately by
stdlib macro-expansion utilities.

## Error Cases

- `(quasiquote (unquote))` => `syntax`: `"unquote needs argument"`
- `(quasiquote (unquote 1 2))` => `syntax`: `"unquote expects a single argument"`
- `(quasiquote (unquote-splicing))` => `syntax`: `"unquote-splicing needs argument"`
- `(quasiquote (unquote-splicing [1] [2]))` => `syntax`: `"unquote-splicing expects a single argument"`
- `read-string ","` => `read`: `"missing argument to , (unquote) operator"`

## Related

- Binding-pattern quasiquote semantics (`bind`) are documented in `docs/bind.md`.
