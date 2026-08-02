  $ cat >test.ml <<'EOF'
  > module Foo = struct
  >   type t = [`A | `B | `C] [@@deriving_inline jsont]
  > 
  >   let _ = fun (_ : t) -> ()
  > 
  >   let jsont = Jsont.enum ~kind:"T" [("A", `A); ("B", `B); ("C", `C)]
  > 
  >   let _ = jsont
  > 
  >   [@@@ppxlib.inline.end]
  > end
  > EOF

  $ cat >dune <<'EOF'
  > (library
  >  (name test)
  >  (preprocess
  >   (pps ppx_deriving_jsont)))
  > EOF
  $ cat >dune-project <<'EOF'
  > (lang dune 3.17)
  > EOF

  $ dune build
  File "test.ml", lines 1-11, characters 0-3:
   1 | module Foo = struct
   2 |   type t = [`A | `B | `C] [@@deriving_inline jsont]
   3 | 
   4 |   let _ = fun (_ : t) -> ()
   5 | 
  ...
   8 |   let _ = jsont
   9 | 
  10 |   [@@@ppxlib.inline.end]
  11 | end
  Error: The type of this module,
         sig
           type t = [ `A | `B | `C ]
           val jsont : ([> `A | `B | `C ] as '_weak1) Jsont.t
         end, contains non-generalizable type variable(s).
         (see manual section 6.1.2)
  File "test.ml", line 6, characters 6-11:
  6 |   let jsont = Jsont.enum ~kind:"T" [("A", `A); ("B", `B); ("C", `C)]
            ^^^^^
    The type of this value, ([> `A | `B | `C ] as '_weak1) Jsont.t,
    contains the non-generalizable type variable(s) '_weak1.
  [1]
