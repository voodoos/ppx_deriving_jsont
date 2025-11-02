open Ppxlib
open! Ast_helper
open Analysis
open Names

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

let jsont_rec_value label = "jsont_rec__" ^ label

let jsont_enum ~loc ~kind assoc =
  let open Ast_builder.Default in
  pexp_apply ~loc (evar ~loc "Jsont.enum")
    [ (Labelled "kind", estring ~loc kind); (Nolabel, assoc) ]

let jsont_sig_item ~loc ~name type_ =
  let open Ast_builder.Default in
  let value_description =
    value_description ~loc
      ~name:(Loc.make ~loc @@ jsont_name name)
      ~type_ ~prim:[]
  in
  psig_value ~loc value_description

let rec of_core_type ~current_decls (core_type : Parsetree.core_type) =
  let of_core_type = of_core_type ~current_decls in
  let loc = core_type.ptyp_loc in
  let nonrecursive = Set.empty in
  (* TODO we should provide finer user control for handling int and floats *)
  match core_type with
  | [%type: unit] -> ([%expr Jsont.null ()], nonrecursive)
  | [%type: string] -> ([%expr Jsont.string], nonrecursive)
  | [%type: bool] -> ([%expr Jsont.bool], nonrecursive)
  | [%type: float] -> ([%expr Jsont.number], nonrecursive)
  | [%type: int] -> ([%expr Jsont.int], nonrecursive)
  | [%type: int32] -> ([%expr Jsont.int32], nonrecursive)
  | [%type: int64] -> ([%expr Jsont.int64], nonrecursive)
  | [%type: [%t? typ] option] ->
      let jsont, requires = of_core_type typ in
      ([%expr Jsont.option [%e jsont]], requires)
  | [%type: [%t? typ] list] ->
      let jsont, requires = of_core_type typ in
      ([%expr Jsont.list [%e jsont]], requires)
  | [%type: [%t? typ] array] ->
      let jsont, requires = of_core_type typ in
      ([%expr Jsont.array [%e jsont]], requires)
  | { ptyp_desc = Ptyp_constr ({ txt = lid; loc }, args); _ } ->
      (* TODO: quoting ? *)
      let requires =
        match lid with
        | Lident name when List.mem (jsont_name name) current_decls ->
            Set.singleton name
        | _ -> nonrecursive
      in
      let is_rec = not (Set.is_empty requires) in
      let args, requires =
        List.fold_left
          (fun (args, requires) arg ->
            let expr, requires' = of_core_type arg in
            (expr :: args, Set.union requires requires'))
          ([], requires) args
      in
      let expr =
        if is_rec then
          let name = jsont_rec_value (Longident.name lid) in
          [%expr Jsont.rec' [%e Exp.ident (Loc.make ~loc (Lident name))]]
        else
          Exp.ident
            (Loc.make ~loc
               (Ppxlib.Expansion_helpers.mangle_lid (Suffix "jsont") lid))
      in
      let expr =
        match (args, is_rec) with
        | _ :: _, false ->
            Exp.apply expr
              (List.map (fun arg -> (Nolabel, arg)) (List.rev args))
        | _ -> expr
      in
      (expr, requires)
  | { ptyp_desc = Ptyp_var label; ptyp_loc; _ } ->
      ( Exp.ident (Loc.make ~loc:ptyp_loc (Lident (jsont_type_var label))),
        nonrecursive )
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
let of_record_type ~current_decls ~loc ~kind labels =
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
      (fun (acc, requires)
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
        let type_jsont, requires' = of_core_type ~current_decls pld_type in
        let requires = Set.union requires requires' in
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
          requires ))
      ( [%expr Jsont.Object.map ~kind:[%e estring ~loc kind] [%e make_fun]],
        Set.empty )
      labels
  in
  ([%expr Jsont.Object.finish [%e mems]], rec_flag)

type decl = {
  type_name : string with_loc;
  type_params : string with_loc list;
  requires : Set.t;
  jsont_expr : expression;
}

let of_type_declaration ~derived_item_loc ~current_decls _rec_flag
    ({ ptype_name; ptype_params; ptype_kind; ptype_manifest; _ } :
      Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  let kind = String.capitalize_ascii ptype_name.txt in
  let type_params =
    List.filter_map
      (fun (core_type, _) ->
        match core_type.ptyp_desc with
        | Ptyp_var label ->
            Some { txt = jsont_type_var label; loc = core_type.ptyp_loc }
        | _ -> None)
      ptype_params
  in
  (* let jsont_value_binding =
    jsont_value_binding ~loc ~params ~name:ptype_name.txt
  in *)
  let requires, jsont_expr =
    match ptype_kind with
    | Ptype_variant constrs
      when List.for_all
             (fun { pcd_args; _ } -> pcd_args = Pcstr_tuple [])
             constrs ->
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
        (Set.empty, jsont_enum ~loc ~kind (elist ~loc all_constrs))
    | Ptype_variant constrs ->
        let open Ast_builder.Default in
        let constrs, rec_flag =
          List.fold_left
            (fun (acc, requires) ({ pcd_name; pcd_vars = _; pcd_args; _ } as cd)
               ->
              let name =
                Attribute.get Attributes.cd_key cd
                |> Option.value ~default:pcd_name.txt
              in
              let arg, requires' =
                match pcd_args with
                | Pcstr_tuple [] -> ([%expr Jsont.null ()], Set.empty)
                | Pcstr_tuple (first :: _) -> of_core_type ~current_decls first
                | Pcstr_record _labels ->
                    failwith
                      "ppx_deriving_jsont: not implemented: inline_records"
              in
              let requires = Set.union requires requires' in
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
              ((binding_name, cd, result) :: acc, requires))
            ([], Set.empty) constrs
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
                     Jsont.Object.Case.value [%e evar ~loc binding_name]
                       [%e var]]
                 in
                 case ~lhs:(pconstruct cd pat) ~guard:None ~rhs)
               constrs
        in
        let cases = elist ~loc cases in
        let expr =
          pexp_let ~loc Nonrecursive bindings
            [%expr
              Jsont.Object.map
                ~kind:[%e estring ~loc:ptype_name.loc kind]
                Fun.id
              |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id
                   ~enc_case:[%e enc_case] [%e cases]
              |> Jsont.Object.finish]
        in
        (rec_flag, expr)
    | Ptype_record labels ->
        let expr, requires = of_record_type ~current_decls ~loc ~kind labels in
        (requires, expr)
    | Ptype_abstract -> (
        match ptype_manifest with
        | Some core_type ->
            let value, requires = of_core_type ~current_decls core_type in
            (requires, value)
        | _ -> failwith "ppx_deriving_jsont: not implemented: abstract types")
    | _ -> failwith "ppx_deriving_jsont: not implemented"
  in
  { type_name = ptype_name; type_params; requires; jsont_expr }
(* jsont_value_binding ~rec_flag jsont_expr *)

let jsont_value_binding ~loc decls =
  let open Ast_builder.Default in
  let is_self_rec decl =
    let rec aux decl ~already_checked requires =
      Printf.eprintf "Is %S rec in (%s) ?\n%!" decl.type_name.txt
        (String.concat ";" (Set.to_list requires));
      if Set.mem decl.type_name.txt requires then (
        Printf.eprintf "%S is rec !\n%!" decl.type_name.txt;
        true)
      else
        (* This could be made more efficient if required *)
        Set.find_first_opt
          (fun required_decl_name ->
            Printf.eprintf "Checking %S rec ?\n%!" required_decl_name;
            if Set.mem required_decl_name already_checked then false
            else
              let required_decl =
                List.find
                  (fun { type_name = { txt = name; _ }; _ } ->
                    String.equal name required_decl_name)
                  decls
              in
              let already_checked =
                Set.add required_decl_name already_checked
              in
              Printf.eprintf "Go aux ! (%S)\n%!" required_decl_name;
              aux decl ~already_checked required_decl.requires)
          requires
        |> Option.fold ~none:false ~some:(fun _ -> true)
    in
    aux decl ~already_checked:Set.empty decl.requires
  in
  (* TODO special case for when there is only one binding *)
  let expr =
    let bindings, values =
      List.map
        (fun decl ->
          let is_self_rec = is_self_rec decl in
          let txt = jsont_rec_value decl.type_name.txt in
          let pat = ppat_var ~loc { decl.type_name with txt } in
          let expr =
            if is_self_rec then [%expr lazy [%e decl.jsont_expr]]
            else decl.jsont_expr
          in
          let expr =
            List.fold_left
              (fun acc label ->
                pexp_fun ~loc:Location.none Nolabel None
                  (ppat_var ~loc:label.loc label)
                  acc)
              expr
              (List.rev decl.type_params)
          in
          let value =
            let ident =
              pexp_ident ~loc { decl.type_name with txt = Lident txt }
            in
            if is_self_rec then [%expr Lazy.force [%e ident]] else ident
          in
          (value_binding ~loc ~pat ~expr, value))
        decls
      |> List.split
    in
    let rec_flag =
      (* Are some of these declaration cross-references ?
         Note: this is weaker than being mutually recursive *)
      let is_rec =
        List.exists (fun { requires; _ } -> not (Set.is_empty requires)) decls
      in
      if is_rec then Recursive else Nonrecursive
    in
    pexp_let ~loc rec_flag bindings (pexp_tuple ~loc values)
  in
  let names =
    ppat_tuple ~loc
    @@ List.map
         (fun { type_name; _ } -> pvar ~loc (jsont_name type_name.txt))
         decls
  in
  value_binding ~loc ~pat:names ~expr

let of_type_declarations ~derived_item_loc rec_flag tds =
  let open Ast_builder.Default in
  let _ = decl_infos tds in
  let names =
    List.map
      (fun { ptype_name = { txt = type_name; _ }; _ } -> jsont_name type_name)
      tds
  in
  let decls =
    List.map
      (of_type_declaration ~derived_item_loc ~current_decls:names rec_flag)
      tds
  in
  (* Currently, we always consider multiple declarations to be mutually
  recursive, but there is enough information in the `requires` field to do finer
  analysis. *)
  (* Similarly we estimate that they all need the complete set of type parameters *)
  pstr_value ~loc:derived_item_loc Nonrecursive
    [ jsont_value_binding ~loc:derived_item_loc decls ]

let sig_of_type_decl ~derived_item_loc
    ({ ptype_name = { txt = name; _ }; _ } : Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  [
    jsont_sig_item ~loc ~name
      [%type: [%t Typ.constr (Loc.make ~loc @@ lident name) []] Jsont.t];
  ]

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let rec_flag =
    if List.length type_declarations > 1 then Recursive else Nonrecursive
  in
  [ of_type_declarations ~derived_item_loc rec_flag type_declarations ]

let generate_sig ~ctxt (_, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (sig_of_type_decl ~derived_item_loc) type_declarations

let _jsont : Deriving.t =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  let sig_type_decl = Deriving.Generator.V2.make_noarg generate_sig in
  Deriving.add "jsont" ~str_type_decl ~sig_type_decl
