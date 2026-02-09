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

## Class System

- Classes are maps carrying `%name`, `%schema`, and `%invoke`; `class?` checks this shape.
- `(class Name [:field Spec {:required true} ...])` returns a class value and does not bind `Name`.
- `(defclass Name ...)` is shorthand for `(def Name (class Name ...))`.
- Construct instances by invoking a class with a map, e.g. `(Name {:field value})`.
- Field spec kinds:
  - a runtime type (`Integer`, `String`, etc.)
  - another class (nested coercion)
  - enum vector syntax: `[:enum :a :b ...]`
- `{:required true}` checks key presence, not non-`nil` value.
- If a key is present with `nil`, the value is accepted and schema checks are skipped for that field.
- Instances are plain maps with their prototype set to the class map.
- Methods are attached to classes as symbol-keyed functions and are usually defined with:
  - `(defmethod ClassName.method [self ...] ...)`
- Calling `obj.method` uses member-call sugar and passes `obj` as the first argument.

## Iterator Exhaustion

- `iter` returns `nil` for empty collections.
- `next` returns `nil` when an iterator is exhausted.
- Calling `deref` or `next` on a consumed iterator object raises an exception.
