type enum = A | X [@key "B"] | C [@@deriving jsont]
type var = V of enum [@key "V2"] | D [@@deriving jsont]
type 'a t = { parent : 'a t option; [@option] v : 'a } [@@deriving jsont]

let print_and_recode j t =
  let open Jsont_bytesrw in
  let json = encode_string j t |> Result.get_ok in
  print_endline json;
  decode_string j json |> Result.get_ok

let v = { parent = Some { parent = None; v = V X }; v = D }
let () = assert (v = print_and_recode (jsont var_jsont) v)
