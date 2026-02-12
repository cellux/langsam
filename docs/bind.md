# Binding and Destructuring

This document describes how Langsam binding works across `let`, function parameters,
`if-match`, and `if-let`.

## Core Model

- Binding is powered by `bind`, which matches a `pattern` against a `value` and writes
  symbol bindings into an environment map.
- A successful bind returns the target environment.
- A failed match returns a `bind` exception.
- Invalid pattern syntax returns a `syntax` exception.
- Literal patterns are matched with `=`.

`bind` is exposed directly:

```lisp
(bind env pattern value)
```

## Where This Semantics Is Used

- `let` binding pairs
- `if-match` binding pairs
- `if-let` binding pairs (with extra truthiness short-circuiting)
- function parameter binding (`fn`, `defn`, `Function`)
- `def` lhs patterns
- `catch` clause patterns

## Pattern Reference

### Symbols and Literals

- Symbol pattern: binds directly.

```lisp
a        ; binds a to rhs
```

- Non-symbol atom pattern (`Integer`, `:kw`, `5`, etc.): literal match with `=`.

```lisp
5        ; matches rhs only when (= 5 rhs)
```

### Vector Patterns

Vector patterns consume the rhs through `iter`.

```lisp
[a b c]
[a b & rest]
[a b &opt c (d :default) (e :default e-set?) & rest]
```

Rules:

- Positional entries bind left-to-right.
- If rhs runs out during required positional binding: `bind` failure.
- Extra rhs items are ignored unless captured with `&`.
- `&` captures all remaining rhs items as a proper list.
- `&` supports a single following pattern form; additional forms are a syntax error.
- `&opt` starts optional bindings:
  - `sym`: default is `nil`
  - `(pat default)`: uses `default` when arg is absent
  - `(pat default symsetp)`: also binds `symsetp` to `true` when arg was passed,
    `false` when default was used
- `&opt` must be followed by at least one optional parameter form.
- optional default forms must be exactly `(pat default)` or
  `(pat default symsetp)` (`symsetp` must be a symbol).
- Optional defaults are evaluated in the current binding environment, so later defaults
  can use earlier bindings.

### Map Patterns

Map patterns destructure by lookup:

```lisp
{a :a b :b}
{:keys [a c (b 2)]}
```

Rules:

- General entry `{pat key}`:
  - lookup `key` from rhs via `get`
  - bind the resulting value to `pat`
- `:keys` entry:
  - each item must be `sym` or `(sym default)`
  - key looked up is the keyword form of `sym` (for `a`, lookup `:a`)
- rhs must be associative (`get` supported), or binding fails.
- For map rhs values, key presence is checked via map entry lookup (including proto chain):
  - missing key -> default may apply
  - present key with `nil` -> treated as present (default does not apply)
- For non-map associative rhs values, presence is inferred from `get`; this cannot
  distinguish "missing" from "present with nil".

### Cons/Special Patterns

Special cons-headed binding patterns:

- `(cons p1 p2 ...)`: destructures cons structure.
- `(and p1 p2 ...)`: all patterns must match same rhs.
- `(or p1 p2 ...)`: first matching pattern wins.
- `(pred f)`: predicate function `f` must return truthy for rhs.
- `(guard expr)`: evaluates `expr` in current binding env; must be truthy.
- `(Type pat)`: rhs type must be exactly `Type`, then `pat` is applied.

### Quote and Quasiquote Patterns

- `(quote x)` / `'x`: literal match.
- `(quasiquote x)` / `` `x `` supports:
  - `,pat` (`unquote`) to bind against a subvalue
  - `,@pat` (`unquote-splicing`) in sequence patterns to bind remaining tail

Examples:

```lisp
`(a b ,c d)
`(a b ,@[x y & rest])
```

## Form-Specific Behavior

### `let`

```lisp
(let [pat1 expr1
      pat2 expr2
      ...]
  body...)
```

Rules:

- Creates a new local env whose prototype is the current env.
- Binding pairs are processed left-to-right.
- Each rhs expression is evaluated before binding its pattern.
- Later rhs expressions can use earlier bindings from the same `let`.
- Odd number of binding forms is a `syntax` error (`incomplete let bindings`).
- Any binding error aborts the `let`.

### Function Parameters

Function params use the same binder as `let`, with args list as rhs.

```lisp
(fn [x y &opt (z (+ x y)) & rest] ...)
```

Rules:

- Parameters are bound at call time in a fresh function env.
- The function env prototypes to the function's captured lexical env.
- `&opt` defaults are evaluated at call time in that env.
- The same vector-pattern rules apply (`&`, `&opt`, destructuring, nested patterns).

### `if-match`

```lisp
(if-match [pat1 expr1
           pat2 expr2]
  then-expr
  else-expr)
```

Rules:

- Uses a fresh local env (like `let`) for attempted bindings.
- Processes pairs left-to-right.
- On successful matching of all pairs, evaluates `then-expr` in that env.
- On `bind`-kind match failure, evaluates `else-expr`.
- Non-`bind` errors (for example `syntax`) propagate instead of falling back.

### `if-let`

```lisp
(if-let [pat1 expr1
         pat2 expr2]
  then-expr
  else-expr)
```

Rules:

- Also uses a fresh local env.
- For each pair, rhs is evaluated first.
- If any rhs is falsey, binding stops immediately and `else-expr` runs.
- If rhs is truthy, pattern binding is attempted.
- Pattern binding errors are not treated as "no match"; they propagate.

Practical implication:

- `if-let` is primarily a truthy guard form.
- `if-match` is the pattern-match fallback form.

## Failure Summary

- Match failures are `bind` exceptions.
- Invalid binder syntax is `syntax`.
- `if-match` and `catch` treat only `bind` failures as recoverable "no match".
- `let`, function calls, and `if-let` propagate binding errors.
