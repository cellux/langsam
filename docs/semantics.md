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

## Stdlib Helper Naming

- Core stdlib helper functions prefixed with `-` are private implementation details.
- `-`-prefixed helpers are not part of the stable public API and may change without compatibility guarantees.

## Map Presence and Value

- `get map key` returns the value or `nil`.
- `contains? map key` returns `true` when the key exists (including proto lookup), even if the value is `nil`.
- `gep map key` returns the raw entry pair `(key . value)` or `nil`.

Use `contains?` for presence checks when `nil` is a valid stored value.

## `reset` and `swap`

- `reset` is a low-level alias of `setcdr`.
- `reset` expects a `Cons` cell, mutates its `cdr`, and returns `nil`.
- A raw map entry `(key . value)` is also a `Cons` cell, typically obtained from `gep`.
- `gep map key` may return:
  - a direct map entry pair
  - an inherited entry pair found via prototype lookup
  - `nil` when the key is missing everywhere
- Typical binding updates use `gep` to obtain that entry pair, for example:
  - `(def ref (gep (curlet) 'x))` where `ref` is `('x . current-value)`.
  - `(reset ref value)` mutates the same pair to `('x . value)`.
- If `gep` returns `nil`, calling `reset`/`swap` on that result raises (because no `Cons` cell is available to mutate/deref).
- `deref` applied to a `Cons` cell returns its cdr.
- `@x` is shorthand for `(deref x)`.
- `swap` expects a `Cons` cell, applies a function to `@ref` (the current `cdr` value), stores the result via `reset`, and forwards optional extra args to that function.
- `swap` returns the new stored value.

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

- Classes are maps marked with a truthy `%class` field (and typically carry `%name`, `%schema`, and `%invoke`); `class?` checks the `%class` marker.
- `(make-class Name [:field Spec {:required true} ...])` returns a class value and does not bind `Name`.
- Class definitions may start with an options map:
  - `(make-class Child {:extends Parent} [:field Spec] ...)`
- `(defclass Name ...)` is shorthand for `(def Name (make-class Name ...))`.
- Construct instances by invoking a class with a map, e.g. `(Name {:field value})`.
- `(class x)` returns the class prototype for class instances, otherwise `(type x)`.
- Field spec kinds:
  - a runtime type (`Integer`, `String`, etc.)
  - another class (nested coercion)
  - enum vector syntax: `[:enum :a :b ...]`
- `{:required true}` checks key presence, not non-`nil` value.
- If a key is present with `nil`, the value is accepted and schema checks are skipped for that field.
- Field options may include `:validate`:
  - `Function`: one validator
  - `Vector` of `Function`s: validators run in order
  - validators run when the key is present (including `nil`)
  - any falsey validator result raises `class` payload `"<Class>.<:field> failed validator"`
- Instances are plain maps with their prototype set to the class map.
- Methods are attached to classes as symbol-keyed functions and are usually defined with:
  - `(defmethod ClassName.method [self ...] ...)`
- Calling `obj.method` uses member-call sugar and passes `obj` as the first argument.
- With `:extends`, child classes inherit parent schema and methods.
  - child instances are validated against merged parent+child schema
  - duplicate field names across parent/child are rejected at class definition time
  - class prototype chaining provides method inheritance; child methods can override parent methods

## Multimethods

- `(defmulti Name dispatch-fn)` defines a callable multimethod value bound to `Name`.
- Multimethod `defmethod` uses Clojure-style target+dispatch syntax:
  - `(defmethod Name dispatch-value [args ...] ...)`
- Multimethod dispatch:
  - applies `dispatch-fn` to call arguments
  - looks up an exact dispatch method first
  - falls back to `:default` when present
  - raises `invoke` when no method matches
- `defmethod` remains overloaded:
  - class methods: `(defmethod ClassName.method [self ...] ...)`
  - multimethod methods: `(defmethod Name dispatch-value [args ...] ...)`

## Slices

- `slice` creates views over vectors and strings:
  - `(slice obj start &opt end)`
  - `obj` must be `Vector`, `VectorSlice`, `String`, or `StringSlice`
  - `start`/`end` must be `Integer`
- If `end` is omitted, it defaults to the source length.
- `start` and `end` support negative indexing from the end.
- Bounds are validated against the source length:
  - `start` and `end` must each be in `0..len` (after negative-index normalization)
  - `end < start` is an error
- `VectorSlice` indexing (`get`/`put`) supports negative indexes like `Vector`.
- `VectorSlice` values share backing storage with the base vector:
  - writing through a slice mutates the base vector
  - writing through the base vector is visible through existing slices
  - slicing a slice keeps sharing the same base storage
- `VectorSlice` iteration uses `VectorSliceIterator` and follows normal iterator exhaustion rules.
- `StringSlice` indexing (`get`) supports negative indexes like `String`.
- `StringSlice` is a view of a base string range; converting it with `str` or `String`
  materializes a regular `String`.

## Iterators

- Iteration protocol:
  - `(iter x)` produces an iterator, or `nil` when there is nothing to iterate.
  - `(deref it)` (or `@it`) returns the current item.
  - `(next it)` is shorthand for `(it)` and advances the iterator.
- Canonical loop shape:
  - `(let [it (iter coll)] (while it ... @it ... (setq it (next it))))`
- Built-in iterable values:
  - `nil` and `Cons` (via `ConsIterator`)
  - `Vector` (via `VectorIterator`)
  - `VectorSlice` (via `VectorSliceIterator`)
  - `Map` (via `MapIterator`)
- `String` and `StringSlice` are indexable (`get`, `slice`) but are not iterable.
- Calling `iter` on a non-iterable value raises an `iter` exception.
- Iterator exhaustion:
  - `next` returns `nil` when an iterator is exhausted.
  - calling `deref` or `next` on an already consumed iterator raises.
- Map iteration:
  - default map iteration yields `[key value]` vectors.
  - it iterates only the map's own entries (not inherited prototype entries).
  - entry order is implementation detail and should not be relied on.
- Map protocol hooks:
  - maps may define `%iter`, `%deref`, and `%invoke` to override behavior.
  - these hooks are resolved through prototypes.
  - stdlib `range` uses this mechanism with a map-backed `RangeIterator`.
