type sort = A | X [@key "B"] | C [@@kind "sort"] [@@doc "some doc"] [@@deriving_inline jsont]

let _ = fun (_ : sort) -> ()
let sort_jsont =
  Jsont.enum ~doc:"some doc" ~kind:"sort" [("A", A); ("B", X); ("C", C)]
let _ = sort_jsont

[@@@ppxlib.inline.end]

type t = {
  name : string; [@jsont.doc "The name of the object"]
  maybe_parent : t option; [@option]
  ids : string list; [@default []] [@omit List.is_empty]
  sort : sort; [@key "Sort"]
} [@@kind "T2"]
[@@deriving_inline jsont]

and u = { x : t } [@@kind "U2"]

let _ = fun (_ : t) -> ()
let _ = fun (_ : u) -> ()

let jsont =
  let rec jsont =
    lazy
      (Jsont.Object.finish
         (Jsont.Object.mem "Sort" sort_jsont ~enc:(fun t -> t.sort)
            (Jsont.Object.mem "ids" (Jsont.list Jsont.string)
               ~enc:(fun t -> t.ids) ~dec_absent:[] ~enc_omit:List.is_empty
               (Jsont.Object.mem "maybe_parent"
                  (Jsont.option (Jsont.rec' jsont))
                  ~enc:(fun t -> t.maybe_parent) ~dec_absent:None
                  ~enc_omit:Option.is_none
                  (Jsont.Object.mem "name" ~doc:"The name of the object"
                     Jsont.string ~enc:(fun t -> t.name)
                     (Jsont.Object.map ~kind:"T2"
                        (fun name maybe_parent ids sort ->
                           { name; maybe_parent; ids; sort }))))))) in
  Lazy.force jsont
let _ = jsont

let u_jsont =
  Jsont.Object.finish
    (Jsont.Object.mem "x" jsont ~enc:(fun t -> t.x)
       (Jsont.Object.map ~kind:"U2" (fun x -> { x })))

let _ = u_jsont

[@@@ppxlib.inline.end]
