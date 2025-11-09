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

(* The [nonrec] annotation is respected *)
module M = struct
  type nonrec v = u [@@deriving jsont]
  and u = v
end

(* A recursive case that requires more careful analysis to not produce invalid
code *)
type t2 = int t3 [@@deriving jsont]
and 'a t4 = T3 of 'a t3 | T2 of t2
and 'a t3 = A of 'a

let v = T2 (A 4)
let () = assert (v = print_and_recode (t4_jsont Jsont.int) v)

(* Same with several mutually recursive groups *)
type r1 = R2 of r2 [@@deriving jsont]
and r2 = R1 of r1 | N of n
and s1 = S2 of s2
and s2 = S1 of s1 | Z
and n = S of s1

let v = R2 (N (S (S2 Z)))
let () = assert (v = print_and_recode r1_jsont v)

(* Tuples *)

type tup = int * string

let get_or_raise = function Ok r -> r | Error err -> raise (Jsont.Error err)

let tup_jsont =
  let enc f acc (i, s) =
    let i = Jsont.Json.encode' Jsont.int i |> get_or_raise in
    let s = Jsont.Json.encode' Jsont.string s |> get_or_raise in
    f (f acc 0 i) 1 s
  in
  let dec_empty () = (None, None) in
  let dec_add =
   fun i elt (e1, e2) ->
    match i with
    | 0 ->
        let i = Jsont.Json.decode' Jsont.int elt |> get_or_raise in
        (Some i, e2)
    | 1 ->
        let s = Jsont.Json.decode' Jsont.string elt |> get_or_raise in
        (e1, Some s)
    | _ -> Jsont.(Error.msgf Meta.none "Too many elements for tuple.")
  in
  let dec_finish meta _ (a, b) =
    let get_or_raise i o =
      match o with
      | Some v -> v
      | None -> Jsont.Error.msgf meta "Missing tuple member #%i" i
    in
    (get_or_raise 0 a, get_or_raise 1 b)
  in
  Jsont.Array.map ~enc:{ enc } ~dec_empty ~dec_add ~dec_finish Jsont.json
  |> Jsont.Array.array

let v : tup = (42, "quarante-deux")
let () = assert (v = print_and_recode tup_jsont v)
