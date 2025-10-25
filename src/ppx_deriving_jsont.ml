open Ppxlib
open! Ast_helper

module Attributes = struct
  let key ctx =
    Attribute.declare "deriving.jsont.key" ctx
      Ast_pattern.(single_expr_payload (estring __))
      Fun.id

  let cd_key = key Attribute.Context.Constructor_declaration
  let ld_key = key Attribute.Context.Label_declaration

  let default context =
    (* This is for compatibility with [ppx_yojson_conv], [absent] is more
       idiomatic to Jsont. *)
    Attribute.declare "deriving.jsont.default" context
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let absent context =
    Attribute.declare "deriving.jsont.absent" context
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let ld_default = default Attribute.Context.label_declaration
  let ld_absent = absent Attribute.Context.label_declaration

  let omit context =
    Attribute.declare "deriving.jsont.omit" context
      Ast_pattern.(single_expr_payload __)
      Fun.id

  let ld_omit = omit Attribute.Context.label_declaration
  let option context = Attribute.declare_flag "deriving.jsont.option" context
  let ld_option = option Attribute.Context.label_declaration
end

let deriver = "jsont"

let jsont_name type_name =
  match type_name with
  | "t" -> "jsont"
  | _ -> Printf.sprintf "%s_jsont" type_name

let jsont_type_var label = "_jsont_type_var__" ^ label
let jsont_rec_value = "jsont_rec__t"

let jsont_enum ~loc ~kind assoc =
  let open Ast_builder.Default in
  pexp_apply ~loc (evar ~loc "Jsont.enum")
    [ (Labelled "kind", estring ~loc kind); (Nolabel, assoc) ]

let jsont_str_item ~loc ~name ~params ~rec_flag expr =
  let open Ast_builder.Default in
  let expr =
    match rec_flag with
    | Nonrecursive -> expr
    | Recursive ->
        [%expr
          let rec [%p ppat_var ~loc { txt = jsont_rec_value; loc }] =
            lazy [%e expr]
          in
          Lazy.force [%e pexp_ident ~loc { txt = Lident jsont_rec_value; loc }]]
  in
  let expr =
    List.fold_left
      (fun acc label ->
        pexp_fun ~loc:Location.none Nolabel None
          (ppat_var ~loc:label.loc label)
          acc)
      expr (List.rev params)
  in
  pstr_value ~loc Nonrecursive
    [ value_binding ~loc ~pat:(pvar ~loc name) ~expr ]

let jsont_sig_item ~loc ~name type_ =
  let open Ast_builder.Default in
  let value_description =
    value_description ~loc
      ~name:(Loc.make ~loc @@ jsont_name name)
      ~type_ ~prim:[]
  in
  psig_value ~loc value_description

let rec of_core_type ~current_decl (core_type : Parsetree.core_type) =
  let of_core_type = of_core_type ~current_decl in
  let loc = core_type.ptyp_loc in
  (* TODO we should provide finer user control for handling int and floats *)
  match core_type with
  | [%type: unit] -> ([%expr Jsont.null ()], Nonrecursive)
  | [%type: string] -> ([%expr Jsont.string], Nonrecursive)
  | [%type: bool] -> ([%expr Jsont.bool], Nonrecursive)
  | [%type: float] -> ([%expr Jsont.number], Nonrecursive)
  | [%type: int] -> ([%expr Jsont.int], Nonrecursive)
  | [%type: int32] -> ([%expr Jsont.int32], Nonrecursive)
  | [%type: int64] -> ([%expr Jsont.int64], Nonrecursive)
  | [%type: [%t? typ] option] ->
      let jsont, rec_flag = of_core_type typ in
      ([%expr Jsont.option [%e jsont]], rec_flag)
  | [%type: [%t? typ] list] ->
      let jsont, rec_flag = of_core_type typ in
      ([%expr Jsont.list [%e jsont]], rec_flag)
  | [%type: [%t? typ] array] ->
      let jsont, rec_flag = of_core_type typ in
      ([%expr Jsont.array [%e jsont]], rec_flag)
  | { ptyp_desc = Ptyp_constr ({ txt = lid; loc }, args); _ } ->
      (* TODO: quoting ? *)
      let rec_flag' =
        match lid with
        | Lident name when jsont_name name = current_decl -> Recursive
        | _ -> Nonrecursive
      in
      let args, rec_flag =
        List.fold_left
          (fun (args, rec_flag) arg ->
            let expr, rec_flag' = of_core_type arg in
            let rec_flag =
              match (rec_flag, rec_flag') with
              | Nonrecursive, Nonrecursive -> Nonrecursive
              | _ -> Recursive
            in
            (expr :: args, rec_flag))
          ([], rec_flag') args
      in
      let expr =
        match rec_flag' with
        | Nonrecursive ->
            Exp.ident
              (Loc.make ~loc
                 (Ppxlib.Expansion_helpers.mangle_lid (Suffix "jsont") lid))
        | Recursive ->
            [%expr
              Jsont.rec' [%e Exp.ident (Loc.make ~loc (Lident jsont_rec_value))]]
      in
      let expr =
        match (args, rec_flag') with
        | _ :: _, Nonrecursive ->
            Exp.apply expr
              (List.map (fun arg -> (Nolabel, arg)) (List.rev args))
        | _ -> expr
      in
      (expr, rec_flag)
  | { ptyp_desc = Ptyp_var label; ptyp_loc; _ } ->
      ( Exp.ident (Loc.make ~loc:ptyp_loc (Lident (jsont_type_var label))),
        Nonrecursive )
  | ct ->
      let msg =
        Printf.sprintf "ppx_deriving_jsont: not implemented: core_type %s"
          (Ppxlib.string_of_core_type ct)
      in
      failwith msg

(* Example from Jsont documentation:
    module Status = struct
      type t = Todo | Done | Cancelled
      let assoc = ["todo", Todo; "done", Done; "cancelled", Cancelled ]
      let jsont = Jsont.enum ~kind:"Status" assoc
    end

    module Item = struct
      type t = { task : string; status : Status.t; tags : string list; }
      let make task status tags = { task; status; tags }
      let task i = i.task
      let status i = i.status
      let tags i = i.tags
      let jsont =
        Jsont.Object.map ~kind:"Item" make
        |> Jsont.Object.mem "task" Jsont.string ~enc:task
        |> Jsont.Object.mem "status" Status.jsont ~enc:status
        |> Jsont.Object.mem "tags" Jsont.(list string) ~enc:tags
          ~dec_absent:[] ~enc_omit:(( = ) [])
        |> Jsont.Object.finish
    end
*)
(* Translates records to javascript objects *)
let of_record_type ~current_decl ~loc ~kind labels =
  let open Ast_builder.Default in
  (* Jsont needs a function to construct the record *)
  let make_fun =
    let record =
      let fields =
        List.map
          (fun { pld_name = { txt = name; loc }; _ } ->
            (Loc.make ~loc @@ lident name, evar ~loc name))
          labels
      in
      pexp_record ~loc fields None
    in
    List.fold_left
      (fun acc { pld_name = { txt = name; loc }; _ } ->
        pexp_fun ~loc Nolabel None (pvar ~loc name) acc)
      record (List.rev labels)
  in
  let mems, rec_flag =
    List.fold_left
      (fun (acc, rec_flag)
           ({ pld_name = { txt = default; loc = name_loc }; pld_type; _ } as ld)
         ->
        let jsont_name =
          Attribute.get Attributes.ld_key ld |> Option.value ~default
        in
        let dec_absent, enc_omit =
          match Attribute.get Attributes.ld_option ld with
          | None -> ([%expr None], [%expr None])
          | Some () -> ([%expr Some None], [%expr Some Option.is_none])
        in
        let dec_absent =
          let absent_or_default =
            (* These have the same meaning. [default] is handled for
               compatibility with ppx_yojson_conv *)
            match Attribute.get Attributes.ld_absent ld with
            | None -> Attribute.get Attributes.ld_default ld
            | Some attr -> Some attr
          in
          match absent_or_default with
          | None -> dec_absent
          | Some e ->
              let loc = e.pexp_loc in
              [%expr Some [%e e]]
        in
        let enc_omit =
          match Attribute.get Attributes.ld_omit ld with
          | None -> enc_omit
          | Some e ->
              let loc = e.pexp_loc in
              [%expr Some [%e e]]
        in
        let type_jsont, rec_flag' = of_core_type ~current_decl pld_type in
        let rec_flag =
          match (rec_flag, rec_flag') with
          | Nonrecursive, Nonrecursive -> Nonrecursive
          | _ -> Recursive
        in
        let field_access =
          let loc = pld_type.ptyp_loc in
          [%expr
            fun t ->
              [%e pexp_field ~loc [%expr t] (Loc.make ~loc @@ lident default)]]
        in
        let loc = ld.pld_loc in
        ( [%expr
            Jsont.Object.mem
              [%e estring ~loc:name_loc jsont_name]
              [%e type_jsont] ~enc:[%e field_access] ?dec_absent:[%e dec_absent]
              ?enc_omit:[%e enc_omit] [%e acc]],
          rec_flag ))
      ( [%expr Jsont.Object.map ~kind:[%e estring ~loc kind] [%e make_fun]],
        Nonrecursive )
      labels
  in
  ([%expr Jsont.Object.finish [%e mems]], rec_flag)

let of_type_declaration ~derived_item_loc
    ({
       ptype_name = { txt = type_name; loc = ptype_name_loc };
       ptype_params;
       ptype_kind;
       ptype_manifest;
       _;
     } :
      Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  let kind = String.capitalize_ascii type_name in
  let current_decl = jsont_name type_name in
  let params =
    List.filter_map
      (fun (core_type, _) ->
        match core_type.ptyp_desc with
        | Ptyp_var label ->
            Some { txt = jsont_type_var label; loc = core_type.ptyp_loc }
        | _ -> None)
      ptype_params
  in
  let jsont_str_item = jsont_str_item ~loc ~params ~name:current_decl in
  match ptype_kind with
  | Ptype_variant constrs
    when List.for_all (fun { pcd_args; _ } -> pcd_args = Pcstr_tuple []) constrs
    ->
      (* Constructors have no argument, we use an enumeration *)
      let open Ast_builder.Default in
      let all_constrs =
        List.map
          (fun ({ pcd_name = { txt = default; loc }; _ } as cd) ->
            let name =
              Attribute.get Attributes.cd_key cd |> Option.value ~default
            in
            [%expr [%e estring ~loc name], [%e econstruct cd None]])
          constrs
      in
      [
        jsont_str_item ~rec_flag:Nonrecursive
          (jsont_enum ~loc ~kind (elist ~loc all_constrs));
      ]
  | Ptype_variant constrs ->
      let open Ast_builder.Default in
      let constrs, rec_flag =
        List.fold_left
          (fun (acc, rec_flag) ({ pcd_name; pcd_vars = _; pcd_args; _ } as cd)
             ->
            let name =
              Attribute.get Attributes.cd_key cd
              |> Option.value ~default:pcd_name.txt
            in
            let arg, rec_flag' =
              match pcd_args with
              | Pcstr_tuple [] -> ([%expr Jsont.null ()], Nonrecursive)
              | Pcstr_tuple (first :: _) -> of_core_type ~current_decl first
              | Pcstr_record _labels ->
                  failwith "ppx_deriving_jsont: not implemented: inline_records"
            in
            let rec_flag =
              match (rec_flag, rec_flag') with
              | Nonrecursive, Nonrecursive -> Nonrecursive
              | _ -> Recursive
            in
            let wrapped_arg =
              let kind =
                estring ~loc:pcd_name.loc (pcd_name.txt ^ "__wrapper")
              in
              [%expr
                Jsont.Object.map ~kind:[%e kind] Fun.id
                |> Jsont.Object.mem "v" [%e arg] ~enc:Fun.id
                |> Jsont.Object.finish]
            in
            let mk_fun =
              (* fun arg -> Circle arg *)
              let loc = pcd_name.loc in
              let pat, var =
                if pcd_args = Pcstr_tuple [] then (punit ~loc, None)
                else
                  let arg_name = "arg" in
                  (pvar ~loc arg_name, Some (evar ~loc arg_name))
              in
              pexp_fun ~loc Nolabel None pat (econstruct cd var)
            in
            let result =
              (* Jsont.Object.Case.map "Circle" Circle.jsont ~dec:circle *)
              let name = estring ~loc:pcd_name.loc name in
              [%expr
                Jsont.Object.Case.map [%e name] [%e wrapped_arg]
                  ~dec:[%e mk_fun]]
            in
            let binding_name = "jsont__" ^ pcd_name.txt in
            ((binding_name, cd, result) :: acc, rec_flag))
          ([], Nonrecursive) constrs
      in
      let bindings, cases =
        List.map
          (fun (binding_name, _, expr) ->
            ( value_binding ~loc ~pat:(pvar ~loc binding_name) ~expr,
              [%expr Jsont.Object.Case.make [%e evar ~loc binding_name]] ))
          constrs
        |> List.split
      in
      let enc_case =
        pexp_function_cases ~loc
        @@ List.map
             (fun (binding_name, cd, _) ->
               let pat, var =
                 if cd.pcd_args = Pcstr_tuple [] then (None, eunit ~loc)
                 else (Some (pvar ~loc "t"), evar ~loc "t")
               in
               let rhs =
                 [%expr
                   Jsont.Object.Case.value [%e evar ~loc binding_name] [%e var]]
               in
               case ~lhs:(pconstruct cd pat) ~guard:None ~rhs)
             constrs
      in
      let cases = elist ~loc cases in
      let expr =
        pexp_let ~loc Nonrecursive bindings
          [%expr
            Jsont.Object.map ~kind:[%e estring ~loc:ptype_name_loc kind] Fun.id
            |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id
                 ~enc_case:[%e enc_case] [%e cases]
            |> Jsont.Object.finish]
      in
      [ jsont_str_item ~rec_flag expr ]
  | Ptype_record labels ->
      let expr, rec_flag = of_record_type ~current_decl ~loc ~kind labels in
      [ jsont_str_item ~rec_flag expr ]
  | Ptype_abstract -> (
      match ptype_manifest with
      | Some core_type ->
          let value, rec_flag = of_core_type ~current_decl core_type in
          [ jsont_str_item ~rec_flag value ]
      | _ -> failwith "ppx_deriving_jsont: not implemented: abstract types")
  | _ -> []

let sig_of_type_decl ~derived_item_loc
    ({ ptype_name = { txt = name; _ }; _ } : Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  [
    jsont_sig_item ~loc ~name
      [%type: [%t Typ.constr (Loc.make ~loc @@ lident name) []] Jsont.t];
  ]

let generate_impl ~ctxt (_, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (of_type_declaration ~derived_item_loc) type_declarations

let generate_sig ~ctxt (_, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (sig_of_type_decl ~derived_item_loc) type_declarations

let _jsont : Deriving.t =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  let sig_type_decl = Deriving.Generator.V2.make_noarg generate_sig in
  Deriving.add "jsont" ~str_type_decl ~sig_type_decl
