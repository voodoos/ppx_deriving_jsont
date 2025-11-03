let print_and_recode j t =
  let open Jsont_bytesrw in
  let json = encode_string j t |> Result.get_ok in
  print_endline json;
  decode_string j json |> Result.get_ok

(* Enums *)
type enum = A | X [@key "B"] | C [@@deriving jsont]

let v = [ A; X; C ]
let () = assert (v = print_and_recode (Jsont.list enum_jsont) v)

(* Record and multiple decls *)
type u = { name : v; next : u option } [@@deriving jsont]
and v = enum

let v : u = { name = A; next = Some { name = X; next = None } }
let () = assert (v = print_and_recode u_jsont v)

(* Variants *)
type b = V of int | U of u | R of { arg : bool } | Empty [@@deriving jsont]

let v = [ V 4; U { name = A; next = None }; R { arg = true }; Empty ]
let () = assert (v = print_and_recode (Jsont.list b_jsont) v)

(* Polymorphic variants *)
type pv = [ `A | `B ] [@@deriving jsont]

let v = [ `A; `B ]
let () = assert (v = print_and_recode (Jsont.list pv_jsont) v)

type pv2 = [ `A of int | `B [@key "'B"] ] [@@deriving jsont]

let v = [ `A 3; `B ]
let () = assert (v = print_and_recode (Jsont.list pv2_jsont) v)

(* Mutually recursive declarations *)
type 'a t = { name : string option; [@option] v : 'a var } [@@deriving jsont]
and 'a var = V of enum [@key "V2"] | D of 'a t | Empty

let v = { name = None; v = D { name = Some "d"; v = Empty } }
let () = assert (v = print_and_recode (jsont jsont) v)
