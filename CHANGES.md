0.3 (unreleased)
===

- Fix weak types issues when deriving empty variant types or polymorphic variant
  types with no parameters. (fixes #1, reported by @mneumann)
- Handle variants with tuples bigger than one - #4 (@patricoferris)

0.2.1
=====

- Replace failwiths by Ppxlib's raise_errorf. This prevents the PPX from
  completely breaking Merlin's pipeline.
- Improved locations for error reporting.
- Prefix usages of Stdlib's function.

0.1.1
=====

- Compatibility with OCaml 4.14.

0.1.0
=====

- First release.
