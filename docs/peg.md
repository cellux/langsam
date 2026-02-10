# PEG Module

`modules/peg.l` implements a Janet-style PEG engine entirely in Langsam.

## Public API

- `(peg/compile peg-src)`
- `(peg/match peg input &opt (start 0) & args)`
- `(peg/find peg input &opt (start 0) & args)`
- `(peg/find-all peg input &opt (start 0) & args)`
- `(peg/replace peg subst input &opt (start 0) & args)`
- `(peg/replacef peg f input &opt (start 0) & args)`

`peg` may be a raw PEG pattern/grammar map or a compiled PEG object.

`input` may be a `String`, `StringSlice`, `StringStream`, `File`, or `FileStream`
through `(stream input)`.

## Consumption Semantics

- `peg/match`:
  anchored at current cursor + `start`.
  success consumes matched bytes; failure consumes nothing.
- `peg/find`:
  scans forward for first match.
  success consumes skipped + matched bytes; failure consumes to EOF.
- `peg/find-all`:
  scans and consumes through EOF while collecting all matches.
- `peg/replace` and `peg/replacef`:
  transform remaining input from cursor + `start`, consume to EOF, and return
  the replaced string.

## Features

Implemented forms include:

- Primitives: string, integer, `true`, `false`, `(range ...)`, `(set ...)`.
- Core combinators and aliases:
  `choice/+`, `sequence/*`, `any`, `some`, `repeat`, `at-least`, `at-most`,
  `between`, `if`, `if-not`, `not/!`, `look/>`, `opt/?`, `to`, `thru`,
  `sub`, `split`, `(n patt)`.
- Captures:
  `capture/<-/quote`, `group`, `replace//`, `constant`, `argument`,
  `position/$`, `line`, `column`, `accumulate/%`, `cmt`, `backref/->`,
  `backmatch`, `unref`, `drop`, `only-tags`, `nth`, numeric captures
  (`uint`, `uint-be`, `int`, `int-be`), `lenprefix`, `number`, and `error`.
- Grammars/recursion:
  grammar maps with `:main`, keyword/symbol rule refs, mutual recursion,
  default aliases (`:d`, `:a`, `:w`, `:s`, `:h`, uppercase negations, and
  `+/*` variants).

`line` and `column` captures are 1-based.
