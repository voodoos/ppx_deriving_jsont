let print_and_recode j t =
  let open Jsont_bytesrw in
  let json = encode_string j t |> Result.get_ok in
  print_endline json;
  decode_string j json |> Result.get_ok

type enum = A | X [@key "B"] | C [@@deriving jsont]

let v = [ A; X; C ]
let () = assert (v = print_and_recode (Jsont.list enum_jsont) v)

type u = { name : v; next : u option } [@@deriving jsont]
and v = enum

let v : u = { name = A; next = Some { name = X; next = None } }
let () = assert (v = print_and_recode u_jsont v)

type 'a t = { name : string option; [@option] v : 'a var } [@@deriving jsont]
and 'a var = V of enum [@key "V2"] | D of 'a t

let v = { name = None; v = D { name = Some "d"; v = V A } }
let () = assert (v = print_and_recode (jsont jsont) v)

(*

let rec jt_rec_t type_param_a =
  lazy ... Jsont.rec' (jt_rec_t type_param_a) ...
in
*)
