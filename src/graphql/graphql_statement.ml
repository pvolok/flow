open Utils_js
open Reason
open Type

module Ast = Graphql_ast
module Flow = Flow_js
module Schema = Graphql_schema

let flow = Flow.gql

type context = {
  mutable vars: Schema.Type.t SMap.t;
  infer_vars: bool;
  relay_op: bool;
}


let rec doc cx doc =
  let schema =
    match Context.graphql_config cx with
    | Some inst -> inst.Graphql_config.schema
    | None -> failwith "impossible"
  in
  match doc.Ast.Document.definitions with
  | [] -> VoidT.at Loc.none
  | def :: rest ->
    List.iter (fun d -> ignore (do_definition cx schema d)) rest;
    do_definition cx schema def

and do_definition cx schema def =
  match def with
  | Ast.Definition.Fragment { Ast.FragmentDef.
      typeCondition = (loc, cond_type);
      _;
    } when Type_inference_hooks_js.dispatch_graphql_type_hook
      schema cond_type loc None
    -> VoidT.at loc
  | Ast.Definition.Fragment { Ast.FragmentDef.
      typeCondition = (type_loc, type_name);
      selectionSet;
      directives;
      loc;
      _
    } ->
    let reason = mk_reason
        (RCustom (spf "fragment on `%s`" type_name)) loc in
    if Graphql_flow.check_frag_type flow cx schema type_name type_loc then
      let gcx = {vars = SMap.empty; infer_vars = true; relay_op = false} in
      let selection =
        mk_selection cx gcx schema type_name false selectionSet in
      let dirs = do_directives
        cx gcx schema Schema.Directive.FragmentDef directives in
      let frag = { Graphql.
        frag_schema = schema;
        frag_type = type_name;
        frag_selection = selection;
        frag_directives = dirs;
      } in
      GraphqlFragT (reason, frag)
    else
      EmptyT.why reason
  | Ast.Definition.Operation op_def ->
    let { Ast.OperationDef.
      operation = (op_loc, operation);
      selectionSet;
      variableDefs;
      directives;
      loc;
      _;
    } = op_def in
    let op_type = Ast.OperationType.(match operation with
      | Query -> Schema.Query
      | Mutation -> Schema.Mutation
      | Subscription -> Schema.Subscription
    ) in
    let (type_name, op_name, dir_loc) = Schema.(
      match op_type with
      | Query ->
          Some schema.Schema.query_name, "query", Schema.Directive.Query
      | Mutation ->
          schema.Schema.mutation_name, "mutation", Schema.Directive.Mutation
      | Subscription ->
          (
            schema.Schema.subscription_name,
            "subscription",
            Schema.Directive.Subscription
          )
    ) in
    let reason = mk_reason (RCustom (spf "GraphQL `%s`" op_name)) loc in
    (match type_name with
    | Some type_name ->
      (* Let's just hardcode Relay behavior for now. If operation contains only
         one field with no args and no selections, we do the following:
         - use args of this field as operation variables
         - do not require selection for this field even if it's object type
       *)
      let relay_op = is_relay_op op_def in
      let (vars, infer_vars) = match variableDefs with
        | Some var_defs -> (do_vars_decl cx schema var_defs, false)
        | None -> (SMap.empty, true)
      in
      let gcx = {vars; infer_vars; relay_op} in
      let selection = mk_selection cx gcx schema type_name false selectionSet in
      let vars = gcx.vars in
      let (_dirs: Graphql.directive list) =
        do_directives cx gcx schema dir_loc directives in
      let operation = { Graphql.
        op_schema = schema;
        op_type;
        op_vars = vars;
        op_selection = selection;
      } in
      GraphqlOpT (reason, operation)
    | None ->
      let loc = Option.value op_loc ~default:Loc.none in
      Flow_error.(add_output cx (EGraphqlUndefOp (loc, op_name)));
      EmptyT.why reason
    )
  | _ ->
    (* TODO: Report an error *)
    VoidT.at Loc.none

and is_relay_op op = Ast.(
  let check_field = function
    | Selection.Field {Field.selectionSet = None; _} -> true
    | _ -> false
  in
  List.for_all check_field op.OperationDef.selectionSet.SelectionSet.selections
)

and do_vars_decl cx schema var_defs =
  let rec conv t =
    match t with
    | Ast.Type.Named (loc, name) ->
      if Schema.type_exists schema name
      then match Schema.type_def schema name with
        | Schema.Type.Scalar _
        | Schema.Type.Enum _
        | Schema.Type.InputObj _
          -> Some (Schema.Type.Named name)
        | _ ->
          Flow_error.(add_output cx (EGraphqlNotInputType (loc, name)));
          None
      else (
        Flow_error.(add_output cx (EGraphqlTypeNotFound (loc, name)));
        None
      )
    | Ast.Type.List (_, t) ->
      Option.map ~f:(fun t -> Schema.Type.List t) (conv t)
    | Ast.Type.NonNull (loc, Ast.Type.NonNull _) ->
      (* `Type!!` *)
      Flow_error.(add_output cx (EGraphqlDoubleNonNull loc));
      None
    | Ast.Type.NonNull (_, t) ->
      Option.map ~f:(fun t -> Schema.Type.NonNull t) (conv t)
  in
  let vars =
    List.fold_left (fun vars var_def ->
      let {
        Ast.VariableDef.variable = (loc, (_, name));
        type_;
        _;
      } = var_def in
      match conv type_ with
      | Some t ->
        if SMap.mem name vars then (
          Flow_error.(add_output cx (EGraphqlVarRedef (loc, name)));
          vars
        ) else SMap.add name t vars
      | None -> vars
    ) SMap.empty var_defs
  in
  vars

and mk_selection cx gcx schema type_name maybe selection_set =
  let {Ast.SelectionSet.selections; loc} = selection_set in
  let reason = mk_reason (RCustom (spf "selection on `%s`" type_name)) loc in
  let s_selections = SMap.from_keys
    (Schema.get_possible_types schema type_name)
    (fun _ -> SMap.empty)
  in
  let selection = { Graphql.
    s_schema = schema;
    s_on = type_name;
    s_selections;
  } in
  select
    cx gcx schema (GraphqlSelectionT (reason, selection)) type_name maybe
    selections

and select cx gcx schema selection type_name maybe selections =
  List.fold_left (fun selection item ->
    match item with
    | Ast.Selection.Field { Ast.Field.
        alias;
        name = (floc, fname);
        args;
        selectionSet;
        directives;
        _
      } ->
      if Type_inference_hooks_js.dispatch_graphql_field_hook
        schema fname floc type_name
      then selection
      else begin
        (* TODO *)
        let reason = mk_reason (RCustom (spf "field `%s`" fname)) floc in
        if Graphql_flow.check_field flow cx schema type_name fname floc then (
          let field_type =
            Schema.find_field_type schema type_name fname in
          let _fields_args =
            let field_def = (Schema.find_field schema type_name fname) in
            let args_def = field_def.Schema.Field.args in
            let args = Option.value args ~default:[] in
            do_args cx gcx schema args_def floc args
          in
          let dirs = do_directives
            cx gcx schema Schema.Directive.Field directives in
          let maybe = maybe || has_maybe_directive dirs in
          let field_selection = do_field_selection
              cx gcx schema
              (Schema.name_of_type field_type)
              maybe
              floc selectionSet in
          let field = {
            Graphql.sf_schema = schema;
            sf_alias = Option.value_map alias ~default:fname ~f:(fun (_, x) -> x);
            sf_name = fname;
            sf_type = field_type;
            sf_maybe = maybe;
            sf_selection = field_selection;
            sf_directives = dirs;
          } in
          let field = GraphqlFieldT (reason, field) in
          Hashtbl.replace (Context.type_table cx) floc field;
          Flow.mk_tvar_where cx (reason_of_t selection) (fun t ->
            let field = Graphql.SelectField field in
            Flow.flow cx (selection, GraphqlSelectT (reason, field, t))
          )
        ) else (
          Option.iter ~f:(skim_selection_set cx gcx schema) selectionSet;
          selection
        )
      end
    | Ast.Selection.InlineFragment { Ast.InlineFragment.
        typeCondition = Some (loc, cond_type);
        _;
      } when Type_inference_hooks_js.dispatch_graphql_type_hook
        schema cond_type loc (Some type_name)
      ->
      selection
    | Ast.Selection.InlineFragment { Ast.InlineFragment.
        typeCondition;
        selectionSet;
        loc = frag_loc;
        directives;
        _;
      }
      ->
      let (frag_type_loc, frag_type) =
        Option.value typeCondition ~default:(Loc.none, type_name) in
      if Graphql_flow.check_frag_type flow cx schema frag_type frag_type_loc
      then
        let reason = mk_reason (RCustom "inline fragment") frag_loc in
        let dirs = do_directives
          cx gcx schema Schema.Directive.InlineFragment directives in
        let maybe = maybe || has_maybe_directive dirs in
        let frag = { Graphql.
          frag_schema = schema;
          frag_type;
          frag_selection =
            mk_selection cx gcx schema frag_type maybe selectionSet;
          frag_directives = dirs;
        } in
        Flow.mk_tvar_where cx (reason_of_t selection) (fun t ->
          let frag = Graphql.SelectFrag (GraphqlFragT (reason, frag)) in
          Flow.flow cx (selection, GraphqlSelectT (reason, frag, t))
        )
      else selection
    | Ast.Selection.FragmentSpread { Ast.FragmentSpread.
        name = (_loc, _name);
        _;
      }
      ->
      Flow.mk_tvar_where cx (reason_of_t selection) (fun t ->
        Flow.flow_t cx (selection, t)
      )
    | Ast.Selection.JS _ -> selection
  ) selection selections

and skim_selection_set cx gcx schema selection_set =
  List.iter (fun item ->
    match item with
    | Ast.Selection.InlineFragment {
        Ast.InlineFragment.typeCondition = Some (type_loc, type_name);
        selectionSet;
        _;
      } ->
      if Graphql_flow.check_frag_type flow cx schema type_name type_loc
      then mk_selection cx gcx schema type_name false selectionSet |> ignore
    | _ -> ()
  ) selection_set.Ast.SelectionSet.selections

and do_field_selection cx gcx schema type_name maybe loc selection_set =
  let need_selection = Schema.Type.(
    match Schema.type_def schema type_name with
    | Obj _ | Interface _ | Union _ -> true
    | _ -> false
  ) in
  match selection_set with
  (* obj { ... } *)
  | Some selection_set when need_selection ->
    Some (mk_selection cx gcx schema type_name maybe selection_set)
  (* scalar { ... } *)
  | Some {Ast.SelectionSet.loc; _} ->
    Flow_error.(add_output cx (EGraphqlNonObjSelect (loc, type_name)));
    None
  (* obj *)
  | None when (need_selection && (not gcx.relay_op)) ->
    Flow_error.(add_output cx (EGraphqlObjNeedSelect (loc, type_name)));
    None
  (* scalar *)
  | None -> None

and do_args cx gcx schema args_def field_loc args =
  let args_map = List.fold_left (fun map arg ->
    let {Ast.Argument.name = (_, name); _} = arg in
    SMap.add name arg map
  ) SMap.empty args in
  SMap.iter (fun _ {Schema.InputVal.name; type_} ->
    match type_ with
    | Schema.Type.NonNull _ when not (SMap.mem name args_map) ->
      err cx field_loc (spf "Missings required argument `%s`" name)
    | _ -> ()
  ) args_def;
  List.fold_left (fun map {Ast.Argument.name = (loc, name); value; _} ->
    if SMap.mem name args_def then (
      let arg_type = (SMap.find name args_def).Schema.InputVal.type_ in
      let v = do_value cx gcx schema arg_type value in
      SMap.add name v map
    ) else (
      err cx loc (spf "Argument `%s` is not found" name);
      map
    )
  ) SMap.empty args

and do_value cx gcx schema type_ value =
  let open Ast.Value in
  let module Ast = Ast in
  let module Val = Schema.Value in
  let type_name = Schema.type_name schema type_ in
  let err cx loc msg = err cx loc msg; Val.Invalid in
  let check_scalar loc input mk_val =
    let err () =
      err cx loc
        (spf "This value expected to be of type `%s`" type_name)
    in
    match Schema.type_def schema type_name with
    | Schema.Type.Scalar (_, kind) ->
      if Schema.can_convert_to input kind
      then mk_val ()
      else err ()
    | _ -> err ()
  in
  match value with
  | Variable (loc, (_, name)) -> do_var cx gcx loc name type_
  | IntValue (loc, str) ->
    begin
      try
        let value = Int32.of_string str |> Int32.to_int in
        check_scalar loc Schema.Type.Int (fun () -> Val.Int value)
      with _ -> err cx loc "This is not a valid int32 value"
    end
  | FloatValue (loc, v) ->
      check_scalar loc Schema.Type.Float (fun () -> Val.Float v)
  | StringValue (loc, v) ->
      check_scalar loc Schema.Type.Str (fun () -> Val.String v)
  | BooleanValue (loc, v) ->
      check_scalar loc Schema.Type.Bool (fun () -> Val.Bool v)
  | NullValue loc ->
    begin match type_ with
    | Schema.Type.NonNull _ -> err cx loc "can't be null"
    | _ -> Val.Null
    end
  | EnumValue (loc, value) ->
    begin match Schema.type_def schema type_name with
    | Schema.Type.Enum (_, values) ->
      if List.exists ((=) value) values then (Val.Enum value)
      else
        err cx loc
          (spf "This value expected to be of enum type `%s`" type_name)
    | _ ->
      err cx loc
        (spf "This value expected to be of type `%s`" type_name)
    end
  | ListValue (loc, items) ->
    let rec check_list = (function
      | Schema.Type.Named _ ->
        err cx loc
          (spf "This value expected to be of type `%s`, found list instead"
             type_name)
      | Schema.Type.List t ->
        Val.List (List.map (do_value cx gcx schema t) items)
      | Schema.Type.NonNull t -> check_list t
    ) in
    check_list type_
  | ObjectValue {Ast.ObjectValue.fields; loc} ->
    begin match Schema.type_def schema type_name with
    | Schema.Type.InputObj (_, field_defs) ->
      let fields_map = List.fold_left (fun map field ->
        let {Ast.ObjectField.name = (loc, name); value; _} = field in
        let v = match SMap.get name field_defs with
          | Some {Schema.InputVal.type_ = field_type; _} ->
            do_value cx gcx schema field_type value
          | None ->
            err cx loc (spf "Field `%s` is not found in `%s`" name type_name)
        in
        SMap.add name v map
      ) SMap.empty fields in
      SMap.iter (fun field_name field_type ->
        let missing =
          match field_type.Schema.InputVal.type_ with
          | Schema.Type.NonNull _ ->
            not (SMap.mem field_name fields_map)
          | _ -> false
        in
        if missing then
          err cx loc
            (spf "Missing required field `%s` in object of type `%s`"
               field_name type_name) |> ignore
      ) field_defs;
      Val.Object fields_map
    | _ ->
      err cx loc "This value expected to be an input object"
    end

and do_var cx gcx loc name type_: Schema.Value.t =
  let mk_val () = Schema.Value.Variable name in
  if SMap.mem name gcx.vars
  then vars_compatible cx loc (SMap.find name gcx.vars) type_ mk_val
  else if gcx.infer_vars then (
    let new_vars = SMap.add name type_ gcx.vars in
    gcx.vars <- new_vars;
    mk_val ()
  ) else (
    err cx loc (spf "Variable `%s` in not declared" name);
    Schema.Value.Invalid
  )

and vars_compatible cx loc t1 t2 mk_val =
  let module T = Schema.Type in
  let rec check t1 t2 =
    match t1, t2 with
    | T.Named n1, T.Named n2 -> n1 = n2
    | T.List t1, T.List t2 -> check t1 t2
    | T.NonNull t1, T.NonNull t2 -> check t1 t2
    | _, _ -> false
  in
  let valid = check t1 t2 in
  if valid then mk_val ()
  else (
    let t_exp = Schema.string_of_type_ref t1 in
    let t_real = Schema.string_of_type_ref t2 in
    err cx loc
      (spf "Expected variable of type `%s`, but got `%s`" t_exp t_real);
    Schema.Value.Invalid
  )

and do_directives cx gcx schema allow_loc = function
  | Some directives ->
    List.fold_left (fun acc directive ->
      let {Ast.Directive.name = (_, name); arguments; loc} = directive in
      match Schema.get_directive schema name with
      | Some { Schema.Directive.args = args_def; locations; _ } ->
        if List.exists ((=) allow_loc) locations then (
          let args = Option.value arguments ~default:[] in
          let dir_args = do_args cx gcx schema args_def loc args in
          {Graphql.dir_name = name; dir_args} :: acc
        ) else (
          err cx loc
            (spf "Directive `%s` is not allowed in this location" name);
          acc
        )
      | None ->
        err cx loc (spf "Directive `%s` is not defined in the schema" name);
        acc
    ) [] directives
  | None -> []

and has_maybe_directive dirs =
  List.exists (fun {Graphql.dir_name; _} ->
    dir_name = "skip" || dir_name = "include"
  ) dirs

and err cx loc msg =
  Flow_error.(add_output cx (EGraphqlCustom (loc, msg)))
