(* Enums *)
type sort = A | X [@key "B"] | C
[@@kind "sort"] [@@doc "some doc"] [@@deriving_inline jsont]

let _ = fun (_ : sort) -> ()

let sort_jsont =
  Jsont.enum ~doc:"some doc" ~kind:"sort" [ ("A", A); ("B", X); ("C", C) ]

let _ = sort_jsont

[@@@ppxlib.inline.end]

(* Tuples *)

type tup = int * string [@@deriving_inline jsont]

let tup_jsont =
  let get_or_raise = function
    | Ok r -> r
    | Error err -> raise (Jsont.Error err)
  in
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

[@@@ppxlib.inline.end]

(* Records *)

type t = {
  name : string; [@jsont.doc "The name of the object"]
  maybe_parent : t option; [@option]
  ids : string list; [@default []] [@omit List.is_empty]
  sort : sort; [@key "Sort"]
}
[@@kind "T2"] [@@doc "A t object"] [@@deriving_inline jsont]

and u = { x : t } [@@kind "U2"]

let _ = fun (_ : t) -> ()
let _ = fun (_ : u) -> ()

let jsont =
  let rec jsont =
    lazy
      (let make name maybe_parent ids sort =
         { name; maybe_parent; ids; sort }
       in
       Jsont.Object.map ~doc:"A t object" ~kind:"T2" make
       |> Jsont.Object.mem "name" ~doc:"The name of the object" Jsont.string
            ~enc:(fun t -> t.name)
       |> Jsont.Object.mem "maybe_parent"
            (Jsont.option (Jsont.rec' jsont))
            ~enc:(fun t -> t.maybe_parent)
            ~dec_absent:None ~enc_omit:Option.is_none
       |> Jsont.Object.mem "ids" (Jsont.list Jsont.string)
            ~enc:(fun t -> t.ids)
            ~dec_absent:[] ~enc_omit:List.is_empty
       |> Jsont.Object.mem "Sort" sort_jsont ~enc:(fun t -> t.sort)
       |> Jsont.Object.finish)
  in
  Lazy.force jsont

let _ = jsont

let u_jsont =
  let make x = { x } in
  Jsont.Object.map ~kind:"U2" make
  |> Jsont.Object.mem "x" jsont ~enc:(fun t -> t.x)
  |> Jsont.Object.finish

let _ = u_jsont

[@@@ppxlib.inline.end]

type v =
  | A of int [@key "Id"] [@kind "One of A kind"]
  | S of sort [@doc "Doc for S"]
  | R of { name : string [@doc "Doc for R.name"] } [@doc "Doc for R"]
[@@doc "Type v"] [@@deriving_inline jsont]

let _ = fun (_ : v) -> ()

let v_jsont =
  let jsont__R =
    Jsont.Object.Case.map "R"
      (let make name = R { name } in
       Jsont.Object.map ~doc:"Doc for R" ~kind:"R" make
       |> Jsont.Object.mem "name" ~doc:"Doc for R.name" Jsont.string
            ~enc:((fun (R t) -> t.name) [@ocaml.warning "-8"])
       |> Jsont.Object.finish)
      ~dec:Fun.id
  and jsont__S =
    Jsont.Object.Case.map "S"
      (Jsont.Object.map ~kind:"S" ~doc:"Doc for S" Fun.id
      |> Jsont.Object.mem "v" ~doc:"Wrapper for S" sort_jsont ~enc:Fun.id
      |> Jsont.Object.finish)
      ~dec:(fun arg -> S arg)
  and jsont__A =
    Jsont.Object.Case.map "Id"
      (Jsont.Object.map ~kind:"One of A kind" Fun.id
      |> Jsont.Object.mem "v" ~doc:"Wrapper for A" Jsont.int ~enc:Fun.id
      |> Jsont.Object.finish)
      ~dec:(fun arg -> A arg)
  in
  Jsont.Object.map ~kind:"V" ~doc:"Type v" Fun.id
  |> Jsont.Object.case_mem "type" ~doc:"Cases for V" Jsont.string ~enc:Fun.id
       ~enc_case:(function
         | R t -> Jsont.Object.Case.value jsont__R (R t)
         | S t -> Jsont.Object.Case.value jsont__S t
         | A t -> Jsont.Object.Case.value jsont__A t)
       [
         Jsont.Object.Case.make jsont__R;
         Jsont.Object.Case.make jsont__S;
         Jsont.Object.Case.make jsont__A;
       ]
  |> Jsont.Object.finish

let _ = v_jsont

[@@@ppxlib.inline.end]
