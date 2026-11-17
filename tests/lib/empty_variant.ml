(* [Jsont.enum []] must not have a weak type *)
type t = | [@@deriving jsont]
