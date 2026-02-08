# Langsam Semantics

This document captures language-level behavior that should remain stable.

## Truthiness

Only the following values are falsey:
- `nil`
- `false`

Everything else is truthy, including:
- `0`, `0.0`
- `""`
- `[]`
- `{}`

## Binding Conditionals

- `if-let` is truthy-binding:
  - evaluates each rhs once
  - if any rhs is falsey, runs the else branch
  - otherwise binds patterns and runs the then branch
- `if-match` is pattern-binding:
  - attempts to bind patterns
  - bind failure runs the else branch

Stdlib wrappers:
- truthy forms: `when-let`, `while-let`, `cond-let`
- match forms: `when-match`, `while-match`, `cond-match`

## Map Presence and Value

- `get map key` returns the value or `nil`.
- `contains? map key` returns `true` when the key exists (including proto lookup), even if the value is `nil`.
- `gep map key` returns the raw entry pair `(key . value)` or `nil`.

Use `contains?` for presence checks when `nil` is a valid stored value.

## Reader Desugaring

Reader output for member/namespace sugar is symbolic AST:
- `foo/bar` => `(foo 'bar)`
- `foo.bar` => `(cons foo 'bar)`

When `(cons foo 'bar)` appears in operator position, it means "call member `bar` on `foo`".
Operationally, Langsam looks up `bar` on `foo` and prepends `foo` as the receiver (first argument).

- `((cons foo 'bar) x y)` behaves like:
  `(let [obj foo] ((get obj 'bar) obj x y))`

## Missing vs `nil`

- Missing map keys read as `nil` through `get`.
- Map destructuring binds missing keys as `nil`.
- In `:keys` destructuring, entries can be `sym` or `(sym default)`;
  defaults are used only when the key is absent.
- Presence checks must use `contains?` to distinguish:
  - missing key
  - present key with `nil` value

## Iterator Exhaustion

- `iter` returns `nil` for empty collections.
- `next` returns `nil` when an iterator is exhausted.
- Calling `deref` or `next` on a consumed iterator object raises an exception.
