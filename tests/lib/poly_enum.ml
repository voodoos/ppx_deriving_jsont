(* Issue #1: enum-like polymorphic variants must not have a weak type *)
type t = [ `A | `B | `C ] [@@deriving jsont]
