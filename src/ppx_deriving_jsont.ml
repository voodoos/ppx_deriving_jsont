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
  let rtag_key = key Attribute.Context.Rtag

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
  (* TODO we should provide finer user control for handling int and floats *)
  match core_type with
  | [%type: unit] -> [%expr Jsont.null ()]
  | [%type: string] -> [%expr Jsont.string]
  | [%type: bool] -> [%expr Jsont.bool]
  | [%type: float] -> [%expr Jsont.number]
  | [%type: int] -> [%expr Jsont.int]
  | [%type: int32] -> [%expr Jsont.int32]
  | [%type: int64] -> [%expr Jsont.int64]
  | [%type: [%t? typ] option] -> [%expr Jsont.option [%e of_core_type typ]]
  | [%type: [%t? typ] list] -> [%expr Jsont.list [%e of_core_type typ]]
  | [%type: [%t? typ] array] -> [%expr Jsont.array [%e of_core_type typ]]
  | { ptyp_desc = Ptyp_constr ({ txt = lid; loc }, args); _ } ->
      (* TODO: quoting ? *)
      let is_rec =
        match lid with
        | Lident name -> (
            match Map.find_opt name current_decls with
            | None -> false
            | Some { self_rec; _ } -> self_rec)
        | _ -> false
      in
      let args = List.map of_core_type args in
      let ident =
        Exp.ident
          (Loc.make ~loc
             (Ppxlib.Expansion_helpers.mangle_lid (Suffix "jsont") lid))
      in
      let with_args =
        match args with
        | _ :: _ ->
            Exp.apply ident
              (List.map (fun arg -> (Nolabel, arg)) (List.rev args))
        | _ -> ident
      in
      let expr =
        if is_rec then [%expr Jsont.rec' [%e with_args]] else with_args
      in
      expr
  | { ptyp_desc = Ptyp_var label; ptyp_loc; _ } ->
      Exp.ident (Loc.make ~loc:ptyp_loc (Lident (jsont_type_var label)))
  | { ptyp_desc = Ptyp_variant (rfs, _, _); ptyp_loc; _ } ->
      let constrs =
        List.filter_map
          (fun ({ prf_desc; _ } as rtag) ->
            match prf_desc with
            | Rinherit _ -> None
            | Rtag (name, empty, cts) ->
                let user_name = Attribute.get Attributes.rtag_key rtag in
                let args =
                  if empty || List.is_empty cts then Pcstr_tuple []
                  else Pcstr_tuple cts
                in
                Some (name, user_name, args))
          rfs
      in
      of_variant_type ~loc:ptyp_loc ~kind:"variant" ~current_decls ~poly:true
        constrs
  | ct ->
      let msg =
        Printf.sprintf "ppx_deriving_jsont: not implemented: core_type %s"
          (Ppxlib.string_of_core_type ct)
      in
      (* TODO better ppx error handling *)
      failwith msg

and of_variant_type ~loc ~kind ~current_decls ?(poly = false)
    (constrs : (string with_loc * string option * constructor_arguments) list) =
  let open Ast_builder.Default in
  let lid name = { name with txt = Lident name.txt } in
  let econstruct name =
    if poly then pexp_variant name.txt
    else
      let lid = lid name in
      pexp_construct lid
  in
  let pconstruct name =
    if poly then ppat_variant name.txt
    else
      let lid = lid name in
      ppat_construct lid
  in
  let as_enum constrs =
    (* Constructors have no argument, we use an enumeration *)
    let all_constrs =
      List.map
        (fun (real_name, user_name, _) ->
          let name = Option.value ~default:real_name.txt user_name in
          let construct = econstruct ~loc real_name None in
          [%expr [%e estring ~loc name], [%e construct]])
        constrs
    in
    jsont_enum ~loc ~kind (elist ~loc all_constrs)
  in
  let as_object_cases constrs =
    let constrs =
      List.fold_left
        (fun acc (real_name, user_name, pcd_args) ->
          let name = Option.value ~default:real_name.txt user_name in
          let arg =
            match pcd_args with
            | Pcstr_tuple [] -> `No_arg
            | Pcstr_tuple [ first ] ->
                `Should_wrap (of_core_type ~current_decls first)
            | Pcstr_tuple (_ :: _) ->
                failwith "ppx_deriving_jsont: not implemented: tuples"
            | Pcstr_record labels ->
                (* Inlined record are tricky because they need to be kept
                     under their type constructor at any time. *)
                let inlined_constr = lid real_name in
                `Inline_record
                  (of_record_type ~current_decls ~loc ~kind ~inlined_constr
                     labels)
            (* failwith
                    "ppx_deriving_jsont: not implemented: inline_records" *)
          in
          let wrapped_arg =
            (* There is no need to wrap when there is no argument *)
            (* TODO We could also detect when the argument is a record and
                 inline it *)
            match arg with
            | `No_arg -> [%expr Jsont.Object.zero]
            | `Inline_record arg -> arg
            | `Should_wrap arg ->
                let kind =
                  estring ~loc:real_name.loc (real_name.txt ^ "__wrapper")
                in
                [%expr
                  Jsont.Object.map ~kind:[%e kind] Fun.id
                  |> Jsont.Object.mem "v" [%e arg] ~enc:Fun.id
                  |> Jsont.Object.finish]
          in
          let mk_fun =
            (* fun arg -> Circle arg *)
            let loc = real_name.loc in
            let pat, var =
              if arg = `No_arg then (punit ~loc, None)
              else
                let arg_name = "arg" in
                (pvar ~loc arg_name, Some (evar ~loc arg_name))
            in
            match arg with
            | `Inline_record _ -> [%expr Fun.id]
            | _ ->
                let construct = econstruct ~loc real_name var in
                pexp_fun ~loc Nolabel None pat construct
          in
          let result =
            (* Jsont.Object.Case.map "Circle" Circle.jsont ~dec:circle *)
            let name = estring ~loc:real_name.loc name in
            [%expr
              Jsont.Object.Case.map [%e name] [%e wrapped_arg] ~dec:[%e mk_fun]]
          in
          let binding_name = "jsont__" ^ real_name.txt in
          (binding_name, real_name, arg, result) :: acc)
        [] constrs
    in
    let bindings, cases =
      List.map
        (fun (binding_name, _, _, expr) ->
          ( value_binding ~loc ~pat:(pvar ~loc binding_name) ~expr,
            [%expr Jsont.Object.Case.make [%e evar ~loc binding_name]] ))
        constrs
      |> List.split
    in
    let enc_case =
      pexp_function_cases ~loc
      @@ List.map
           (fun (binding_name, real_name, arg, _) ->
             let loc = real_name.loc in
             let pat, var =
               if arg = `No_arg then (None, eunit ~loc)
               else (Some (pvar ~loc "t"), evar ~loc "t")
             in
             let wrapped_var =
               match arg with
               | `Inline_record _ -> econstruct ~loc real_name (Some var)
               | _ -> var
             in
             let rhs =
               [%expr
                 Jsont.Object.Case.value [%e evar ~loc binding_name]
                   [%e wrapped_var]]
             in
             let lhs = pconstruct ~loc real_name pat in
             case ~lhs ~guard:None ~rhs)
           constrs
    in
    let cases = elist ~loc cases in
    pexp_let ~loc Nonrecursive bindings
      [%expr
        Jsont.Object.map ~kind:[%e estring ~loc kind] Fun.id
        |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id
             ~enc_case:[%e enc_case] [%e cases]
        |> Jsont.Object.finish]
  in
  if List.for_all (fun (_, _, pcd_args) -> pcd_args = Pcstr_tuple []) constrs
  then as_enum constrs
  else as_object_cases constrs

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
and of_record_type ~current_decls ~loc ~kind ?inlined_constr labels =
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
    let wrapped_record =
      match inlined_constr with
      | None -> record
      | Some lid -> pexp_construct ~loc:lid.loc lid (Some record)
    in
    List.fold_left
      (fun acc { pld_name = { txt = name; loc }; _ } ->
        pexp_fun ~loc Nolabel None (pvar ~loc name) acc)
      wrapped_record (List.rev labels)
  in
  let mems =
    List.fold_left
      (fun acc
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
        let type_jsont = of_core_type ~current_decls pld_type in
        let field_access =
          let loc = pld_type.ptyp_loc in
          let arg =
            let var_t = ppat_var ~loc { txt = "t"; loc } in
            match inlined_constr with
            | None -> var_t
            | Some cstr -> ppat_construct ~loc cstr (Some var_t)
          in
          let pexp_attributes =
            match inlined_constr with
            | None -> []
            | Some _ ->
                let name = { txt = "ocaml.warning"; loc } in
                let payload = PStr [ pstr_eval ~loc (estring ~loc "-8") [] ] in
                [ attribute ~loc ~name ~payload ]
          in
          {
            ([%expr
               fun [%p arg] ->
                 [%e
                   pexp_field ~loc [%expr t] (Loc.make ~loc @@ lident default)]])
            with
            pexp_attributes;
          }
        in
        let loc = ld.pld_loc in
        [%expr
          Jsont.Object.mem
            [%e estring ~loc:name_loc jsont_name]
            [%e type_jsont] ~enc:[%e field_access] ?dec_absent:[%e dec_absent]
            ?enc_omit:[%e enc_omit] [%e acc]])
      [%expr Jsont.Object.map ~kind:[%e estring ~loc kind] [%e make_fun]]
      labels
  in
  [%expr Jsont.Object.finish [%e mems]]

type decl = { infos : decl_infos; jsont_expr : expression }

let of_type_declaration ~derived_item_loc ~current_decls
    ({ ast = { ptype_name; ptype_kind; ptype_manifest; _ }; _ } as infos) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  let kind = String.capitalize_ascii ptype_name.txt in
  let jsont_expr =
    match ptype_kind with
    | Ptype_variant constrs ->
        let constrs =
          List.map
            (fun ({ pcd_name; pcd_args; _ } as cd) ->
              let user_name = Attribute.get Attributes.cd_key cd in
              (pcd_name, user_name, pcd_args))
            constrs
        in
        of_variant_type ~loc ~kind ~current_decls constrs
    | Ptype_record labels ->
        let expr = of_record_type ~current_decls ~loc ~kind labels in
        expr
    | Ptype_abstract -> (
        match ptype_manifest with
        | Some core_type ->
            let value = of_core_type ~current_decls core_type in
            value
        | _ -> failwith "ppx_deriving_jsont: not implemented: abstract types")
    | _ -> failwith "ppx_deriving_jsont: not implemented"
  in
  { infos; jsont_expr }

let jsont_value_binding ~loc ~non_rec (decls : decl Map.t) =
  let open Ast_builder.Default in
  (* TODO special case for when there is only one binding *)
  let bindings, values =
    List.map
      (fun (_, decl) ->
        let txt = jsont_name decl.infos.type_name.txt in
        let pat = ppat_var ~loc { decl.infos.type_name with txt } in
        let expr =
          if decl.infos.self_rec then [%expr lazy [%e decl.jsont_expr]]
          else decl.jsont_expr
        in
        let expr =
          List.fold_left
            (fun acc label ->
              pexp_fun ~loc:Location.none Nolabel None
                (ppat_var ~loc:label.loc label)
                acc)
            expr
            (List.rev decl.infos.type_params)
        in
        let value =
          let ident =
            pexp_ident ~loc { decl.infos.type_name with txt = Lident txt }
          in
          let with_args =
            match decl.infos.type_params with
            | _ :: _ ->
                Exp.apply ident
                  (List.map
                     (fun arg ->
                       ( Nolabel,
                         Exp.ident (Loc.make ~loc:arg.loc (Lident arg.txt)) ))
                     (List.rev decl.infos.type_params))
            | _ -> ident
          in
          let with_lazy =
            if decl.infos.self_rec then [%expr Lazy.force [%e with_args]]
            else with_args
          in
          List.fold_left
            (fun acc param ->
              pexp_fun ~loc Nolabel None (ppat_var ~loc param) acc)
            with_lazy decl.infos.type_params
        in
        (value_binding ~loc ~pat ~expr, value))
      (Map.to_list decls)
    |> List.split
  in
  let is_rec =
    (* Are some of these declaration cross-references ?
      Note: this is weaker than being mutually recursive,
            this is in fact not recursion

       And broken in some not-really recursive cases:
       let rec t = u and u = 4;;
       Error: This kind of expression is not allowed as
       right-hand side of let rec *)
    (not non_rec)
    && Map.exists
         (fun _ { infos = { requires; _ }; _ } -> not (Set.is_empty requires))
         decls
  in
  match bindings with
  (* Special case for lone decls that are not recursive *)
  | [ binding ] when not is_rec -> binding
  | _ ->
      let expr =
        let rec_flag = if is_rec then Recursive else Nonrecursive in
        pexp_let ~loc rec_flag bindings (pexp_tuple ~loc values)
      in
      let names =
        ppat_tuple ~loc
        @@ List.map
             (fun (_, { infos = { type_name; _ }; _ }) ->
               pvar ~loc (jsont_name type_name.txt))
             (Map.to_list decls)
      in
      value_binding ~loc ~pat:names ~expr

let of_type_declarations ~derived_item_loc rec_flag tds =
  let open Ast_builder.Default in
  let non_rec = rec_flag = Nonrecursive in
  let current_decls = decl_infos ~non_rec tds in
  let () =
    if debug then
      Map.iter
        (fun _ infos -> Format.eprintf "%a\n%!" pp_decl_infos infos)
        current_decls
  in
  let decls =
    Map.map (of_type_declaration ~derived_item_loc ~current_decls) current_decls
  in
  (*
    Some type definiton cannot be directly translated to a let rec:

    let rec t = u and u = 4;;
    Error: This kind of expression is not allowed as right-hand side of let rec
  *)
  (* Similarly we estimate that they all need the complete set of type parameters *)
  pstr_value ~loc:derived_item_loc Nonrecursive
    [ jsont_value_binding ~non_rec ~loc:derived_item_loc decls ]

let sig_of_type_decl ~derived_item_loc
    ({ ptype_name = { txt = name; _ }; _ } : Parsetree.type_declaration) =
  (* TODO it would be better to have the loc of the annotation here *)
  let loc = derived_item_loc in
  [
    jsont_sig_item ~loc ~name
      [%type: [%t Typ.constr (Loc.make ~loc @@ lident name) []] Jsont.t];
  ]

let generate_impl ~ctxt (rec_flag, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  [ of_type_declarations ~derived_item_loc rec_flag type_declarations ]

let generate_sig ~ctxt (_, type_declarations) =
  let derived_item_loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map (sig_of_type_decl ~derived_item_loc) type_declarations

let _jsont : Deriving.t =
  let str_type_decl = Deriving.Generator.V2.make_noarg generate_impl in
  let sig_type_decl = Deriving.Generator.V2.make_noarg generate_sig in
  Deriving.add "jsont" ~str_type_decl ~sig_type_decl
