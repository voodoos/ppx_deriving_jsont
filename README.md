# [@@deriving jsont]

`ppx_deriving_jsont` is a [PPX deriver](https://ocaml-ppx.github.io/ppxlib/ppxlib/driver.html#def_derivers) that generates
[Jsont](https://erratique.ch/software/jsont) descriptions of OCaml types. Jsont
allows for a lot more flexibility and precision when writing mappings between
OCaml values and JSON. This PPX does not purposes to be a completely automatic
replacement for manual bindings but rather a tool to help generate tedious parts
of the bindings that can be mix-and-matched with carefully user-written
descriptions when that is necessary.

## 🚧🚧 Work in progress

This an early take on writing a deriver for
[Jsont](https://erratique.ch/software/jsont). It can already be used to skip
tedious mechanical work like describing large records and lists of variants, but
it does not do justice to Jsont's fine control and flexibily over the resulting
mappings (like choosing the way integers are mapped).

Any kind of contribution (bug-report, PR, suggestions) is welcomed! I am in no
way a PPX expert, so there might be a lot of non-idiomatic things here that I'd
be happy to improve.

## Todo / Roadmap / Wishlist

- [x] Variants without type parameters (enum)
- [x] Variants with one type parameter
- [ ] Tuples
- [ ] Variants with more than one type parameter (using tuples)
- [x] Inline records
- [x] Records-as-objects
- [x] Types with parameters
- [x] [Recursive types](https://erratique.ch/software/jsont/doc/cookbook.html#recursion)
- [ ] Mutually recursive types
- [ ] Support for all meaningful base types
- [ ] Options (in the form of attributes)
    - [ ] to pass custom Jsont values
    - [ ] for finer support of integers
    - [ ] for finer settings
    - [x] to provide `doc` comments
    - [ ] for other kinds of objects mappings (as sets for example)
    - [ ] for other kinds of variants mappings (as arrays for example)
- [ ] Also generate objects' Paths (lenses ?)
- [ ] Ensure locations make sense
- [ ] Comprehensive test-suite

## Installation

`ppx_deriving_jsont` is still experimental and has not been released to Opam
yet. Given how incomplete it is right now you might want to vendor it and
eventually contribute your improvements upstream. Alternatively, the development
version can be installed in a switch using Opam's `pin` command:

```shell
opam pin https://github.com/voodoos/ppx_deriving_jsont.git
```

## Configuration

Setup depends of your build system of choice. To enable the deriver in a Dune
library (or executable), one should add a dependency to `jsont` and use the
`ppx_deriving_jsont` preprocessor:

```sexp
(library
 ...
 (libraries jsont ...)
 (preprocess (pps ppx_deriving_jsont)))
```

## Usage

Generation is enabled by adding the `[@@deriving jsont]` attribute to type
declarations.

Generation can be tuned with the use of attributes like `[@key "Key"]` that are
often compatible with other derivers such as
[`ppx_yojson_conv`](https://github.com/janestreet/ppx_yojson_conv). These can
also be prefixed `[@jsont.key ...]` when that compatibility isn't desired.

The deriver follows the usual naming conventions. Types whose name is `t`
generates a value named `jsont`. Otherwise that value bears the name of the type
suffixed by `_jsont`.

### Declaration attributes

All type declarations can be annotated with the `[@@kind "Some kind"]` and
`[@@doc "Some doc"]` attributes to improve error messages. This has no effect
when used on base types.

### Basic types (with parameters)

#### Example

```ocaml
type 'a t = 'a [@@deriving jsont]

type u = int list t [@@deriving jsont]
```

<details><summary>See generated code</summary></h3>

```ocaml
let jsont jsont_type_var__a = jsont_type_var__a

let u_jsont = jsont (Jsont.list Jsont.int)
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string u_jsont [3; 6; 4; 2];;
```

```json
[3,6,4,2]
```

### Enumerations

⚠️ Only variants whose constructors have no type parameters are translated as enumerations.

#### Attributes
- `@key <string>` specifies the JSON name (otherwise the same as the
  constructor itself)

#### Example

```ocaml
type sort = A | X [@key "B"] | C [@@deriving jsont]
```

<details><summary>See generated code</summary>

```ocaml
let sort_jsont = Jsont.enum ~kind:"Sort" [ ("A", A); ("B", X); ("C", C) ]
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string (Jsont.list u_jsont) [ A; X; C ];;
```

```json
["A","B","C"]
```

### Generic variants

#### Attributes
- `@key <string>` specifies the JSON name (otherwise the same as the
  constructor itself)

#### Example

```ocaml
type v = A of int [@key "Id"] | S of sort [@@deriving jsont]
```

<details><summary>See generated code</summary>

```ocaml


```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string (Jsont.list v_jsont) [ S X; A 42 ];;
```

```json
[{"type":"S","v":"B"},{"type":"Id","v":42}]
```

### Records

Records are mapped using the ["objects-as-records"
technique](https://erratique.ch/software/jsont/doc/cookbook.html#objects_as_records).

#### Attributes
- `@key <string>` specifies the JSON key (otherwise the same as the
  field)
- `@doc <string>` to document fields
- `@absent <expr>` / `@default <expr>` specifies the value to use when decoding
  if the field is absent (see [the cookbook](https://erratique.ch/software/jsont/doc/cookbook.html#optional_members))
- `@omit <expr: unit -> bool>` specifies when a value should be ommitted during encoding  [the cookbook](https://erratique.ch/software/jsont/doc/cookbook.html#optional_members)
- `@option` is a shorcut for `@absent None`  and `@omit Option.is_none`

#### Example

```ocaml
type t = {
  name : string;
  maybe_parent : t option; [@option]
  ids : string list; [@default []] [@omit List.is_empty]
  sort : sort; [@key "Sort"]
}
[@@deriving jsont]
```

<details><summary>See generated code</summary>

```ocaml
let jsont =
  let rec jsont_rec__t =
    lazy
      (Jsont.Object.finish
         (Jsont.Object.mem "Sort" sort_jsont
            ~enc:(fun t -> t.sort)
            ?dec_absent:None ?enc_omit:None
            (Jsont.Object.mem "ids" (Jsont.list Jsont.string)
               ~enc:(fun t -> t.ids)
               ?dec_absent:(Some []) ?enc_omit:(Some List.is_empty)
               (Jsont.Object.mem "maybe_parent"
                  (Jsont.option (Jsont.rec' jsont_rec__t))
                  ~enc:(fun t -> t.maybe_parent)
                  ?dec_absent:(Some None) ?enc_omit:(Some Option.is_none)
                  (Jsont.Object.mem "name" Jsont.string
                     ~enc:(fun t -> t.name)
                     ?dec_absent:None ?enc_omit:None
                     (Jsont.Object.map ~kind:"T"
                        (fun name maybe_parent ids sort ->
                          { name; maybe_parent; ids; sort })))))))
  in
  Lazy.force jsont_rec__t
```

</details>

#### Json output:

```ocaml
# Jsont_bytesrw.encode_string (Jsont.list u_jsont)
       {
         name = "Alice";
         maybe_parent = Some {
            name = "Bob";
            maybe_parent = None;
            ids = [ "X" ];
            sort = X };
         ids = [];
         sort = A;
       };;
```

```json
{
  "name":"Alice",
  "maybe_parent":
    {"name":"Bob", "ids":["X"], "Sort":"B"},
  "Sort":"A"
}
```
