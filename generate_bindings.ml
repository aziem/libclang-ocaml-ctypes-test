module Oclang = Ocamlclang

let verbose = ref false
let cppmode = ref false
let filename = ref ""

type enum_info =
  {
    name : string;
    enumconstants : string list;
  }

type func_decl =
  {
    name : string;
    resulttype : string;
    paramtypes : string list;
  }

type struct_decl =
  {
    name : string;
    fieldnames : (string * string) list
  }

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i] :: l) in
  expl (String.length s - 1) []

let implode l =
  let result = Bytes.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  Bytes.to_string (imp 0 l)

let normalize_list l =
  let isupper c = (c >= 'A' && c <= 'Z') in
  let tolower c =  Char.chr (Char.code c + 32) in
  let rec aux hump accu = function
    | [] -> List.rev accu
    | h::t when h = '_' || h = '-' ->
        aux true ('_'::accu) t
    | h::t when isupper h && not hump -> (* first hump character *)
        aux true ((tolower h)::'_'::accu) t
    | h::t when isupper h && hump -> (* another hump character *)
        aux hump ((tolower h)::accu) t
    | h::t when h = '.' || h = ':' || h = '/' ->
        aux true (h::accu) t
    | h::t -> (* end of hump *)
        aux false (h::accu) t
  in
  match l with
    | [] -> []
    | h::_ -> aux (isupper h) [] l

let to_snake_case s =
  explode s |> normalize_list |> implode

let rec print_type cur ty =
  let tyname = Oclang.Type.name ty in
  begin
    match Oclang.Type.kind ty with
    | Oclang.Type.Pointer ->
       let ptee = Oclang.Type.get_pointee_type ty in
       let res =  begin
           match Oclang.Type.kind ptee with
           | Oclang.Type.Char_U
             | Oclang.Type.Char_S
             | Oclang.Type.Char16
             | Oclang.Type.Char32 -> Printf.sprintf "T.string"
           | Oclang.Type.Unexposed
             | Oclang.Type.FunctionProto -> Printf.sprintf "(%s)" (print_type cur ptee)
           | _ -> Printf.sprintf "T.ptr (%s)" (print_type cur ptee)
         end
       in
       res
    | Oclang.Type.Void -> Printf.sprintf "T.void"
    | Oclang.Type.UInt -> Printf.sprintf "T.size_t"
    | Oclang.Type.Int -> Printf.sprintf "T.int"
    | Oclang.Type.Bool -> Printf.sprintf "T.bool"
    | Oclang.Type.Char_S 
      | Oclang.Type.Char_U -> Printf.sprintf "T.string"
    | Oclang.Type.Typedef ->
       if (BatString.exists tyname "int") then
         Printf.sprintf "T.%s" tyname
       else
         Printf.sprintf "%s" tyname
    | Oclang.Type.Unexposed ->
       let canonical_type = Oclang.Type.canonical ty in
       print_type cur canonical_type
       
       
    | Oclang.Type.FunctionProto ->
       let retype = Oclang.Type.get_result_type ty in
       
       let paramtypes = List.rev (
         Oclang.Cursor.visit cur
           (fun c p d ->
             match Oclang.Cursor.get_kind c with
             | Oclang.Cursor.ParmDecl ->
                let parmtype = Oclang.Type.of_cursor c in
                let res = print_type c parmtype in
                Oclang.Cursor.Continue, res::d
             | _ -> Oclang.Cursor.Continue, d
           ) [])
       in
       let proto =
         if(List.length paramtypes) = 0 then
           "T.void -> "
         else 
           List.fold_left (fun str pt -> pt ^ " -> " ^ str) "" paramtypes
       in
       proto ^ " returning " ^ (print_type cur retype)
       
    | _ -> Printf.sprintf " %s " (to_snake_case tyname)
  end
  
  
let gather_struct_info cur =
  let _gather_fields cur parent data =
    match Oclang.Cursor.get_kind cur with
    | Oclang.Cursor.FieldDecl ->
       Oclang.Cursor.Recurse, ((Oclang.Cursor.name cur), print_type cur (Oclang.Type.of_cursor cur))::data
    | _ -> Oclang.Cursor.Recurse, data
  in
  let _gather cur =
    match Oclang.Cursor.get_kind cur with
    | Oclang.Cursor.StructDecl ->
       let fields = Oclang.Cursor.visit cur _gather_fields [] in
       let strct =
         {
           name = (Oclang.Cursor.name cur);
           fieldnames = fields;
         }
       in
       Some strct
    | _ -> None
         
  in
  let visitor cur parent data =
    match Oclang.Cursor.cursor_is_in_system_header cur with
    | true -> Oclang.Cursor.Continue, data
    | false ->
       let res = _gather cur in
       match res with
       | Some structdecl -> Oclang.Cursor.Recurse, structdecl::data
       | None -> Oclang.Cursor.Recurse, data
  in
  Oclang.Cursor.visit cur visitor []
  
let gather_func_info cur =
  let _gather_params cur parent data =
    match Oclang.Cursor.get_kind cur with
    | Oclang.Cursor.ParmDecl ->
       let ty = Oclang.Type.of_cursor cur |> print_type cur in
       Oclang.Cursor.Recurse, ty::data
    | _ -> Oclang.Cursor.Recurse, data
  in
  let _gather cur =
    match Oclang.Cursor.get_kind cur with
    | Oclang.Cursor.FunctionDecl ->
       let params = Oclang.Cursor.visit cur _gather_params [] in
       (* Hack - if the no params then assume void is the param type *)
       let params2 = if (List.length params) = 0 then ["T.void"] else params in
       let ret_type = Oclang.Type.of_cursor cur |> Oclang.Type.get_result_type |> print_type cur in
       let f = { name = Oclang.Cursor.name cur; resulttype = ret_type; paramtypes = params2 } in
       Some f
       
    | _ -> None
  in
  
  let visitor cur parent data =
    match Oclang.Cursor.cursor_is_in_system_header cur with
    | true -> Oclang.Cursor.Continue, data
    | false ->
       let res = _gather cur in
       match res with
       | Some funcdecl -> Oclang.Cursor.Recurse, funcdecl::data
       | None -> Oclang.Cursor.Recurse, data
  in
  Oclang.Cursor.visit cur visitor []

let gather_enum_info cur =
  let _gather_enum_decls cur parent data =
    match Oclang.Cursor.get_kind cur with
    | Oclang.Cursor.EnumConstantDecl ->
       Oclang.Cursor.Recurse, (Oclang.Cursor.name cur)::data
    | _ -> Oclang.Cursor.Recurse, data
  in
  
  let _gather cur =
    match Oclang.Cursor.get_kind cur with
    | Oclang.Cursor.EnumDecl ->
       let enumdecllist = List.rev (Oclang.Cursor.visit cur _gather_enum_decls []) in
       let enumdecl = { name = Oclang.Cursor.name cur; enumconstants = enumdecllist } in
       Some enumdecl
    | _ -> None
       
  in
  
  let visitor cur parent data =
    match Oclang.Cursor.cursor_is_in_system_header cur with
    | true -> Oclang.Cursor.Continue, data
    | false ->
       begin
       let res = _gather cur in
       match res with
       | Some enumdecl -> Oclang.Cursor.Recurse, enumdecl::data
       | None -> Oclang.Cursor.Recurse, data
       end
  in
  Oclang.Cursor.visit cur visitor []

let generate_bindings cur =
  let enums = gather_enum_info cur in
  let funcs = gather_func_info cur in
  let structs = gather_struct_info cur in

  Printf.printf "open Ctypes\n";

  List.iter (fun (e: enum_info) ->
      Printf.printf "type %s = \n" e.name;
      List.iter (fun ec -> Printf.printf "\t | %s\n" ec) e.enumconstants;
      Printf.printf "\n") enums;

  List.iter (fun s ->
      Printf.printf "type %s\n" s.name) structs;
  Printf.printf "\n";

  Printf.printf "moule Enums(T:Cstubs_structs.TYPE) = \n struct\n";

  List.iter (fun e ->
      List.iter (fun ec -> Printf.printf "\t let %s = T.constant \"%s\" T.int64_t\n" ec ec) e.enumconstants;
      Printf.printf "\t let %s = T.Enum %s [\n" e.name e.name;
      List.iter (fun ec -> Printf.printf "\t\t %s,%s;\n" ec ec) e.enumconstants;
      Printf.printf "\t ]\n\n"
    )
    enums;

  List.iter (fun s ->
      Printf.printf "\tlet %s : %s Ctypes.structure T.typ = T.structure \"%s\"\n" s.name s.name s.name;
      List.iter (fun (f,t) -> Printf.printf "\tlet %s_%s = T.field %s \"%s\" (%s)\n" s.name f s.name f t) s.fieldnames;
      Printf.printf "\n\n";
    )
    structs;

  Printf.printf "end\n\n";
  Printf.printf "module Bindings(T:Cstubs_structs.TYPE with type 'a typ = 'a typ)(F:Cstubs.FOREIGN) =\n";
  Printf.printf "struct\n";
  Printf.printf "\t module E = Enums(T)\n";
  Printf.printf "\topen F\n";

  List.iter (fun (f:func_decl) ->
      Printf.printf "\t let %s = F.foreign \"%s\" (" f.name f.name;
      List.iter (fun p -> Printf.printf "%s @-> " p) f.paramtypes;
      Printf.printf "returning (%s))\n" f.resulttype
    ) funcs;
  Printf.printf "end\n"

  
  
let new_print_enum (en : enum_info)  =
  Printf.printf "Enum: %s" en.name;
  List.iter (fun ed -> Printf.printf "Enum decl: %s\n" ed) en.enumconstants 

let new_print_func ( fn : func_decl ) =
  Printf.printf "Func: %s" fn.name;
  List.iter (fun paramt -> Printf.printf "Param: %s\n" paramt) fn.paramtypes;
  Printf.printf "Returning %s\n" fn.resulttype

let new_print_struct (st:struct_decl) =
  Printf.printf "Struct: %s" st.name;
  List.iter (fun (fieldname, typestring) -> Printf.printf "Field: %s of type: %s\n" fieldname typestring) st.fieldnames

let () =
  let speclist =
    [ ("-cppmode", Arg.Set cppmode, "Enables c++ mode parsing");
      ("-file", Arg.Set_string filename, "File name to parse");]
  in
  let usage_msg = "Pass a C header file to get some OCaml bindings out" in
  Arg.parse speclist print_endline usage_msg;
  match !filename with
  | "" -> Printf.printf "Error, no file name provided. Use -file <filename>\n"
  | _ ->
    Printf.printf "Clang version is %s\n" (Oclang.Util.version ());
    Printf.printf "Processing File: %s\n" !filename;
    let idex = Oclang.Index.create_index false false in
    let cur =
      Oclang.TranslationIndex.parse_translation_unit ~iscpp:!cppmode idex !filename [] |>
      Oclang.Cursor.cursor_of_translation_unit
    in

    (* let enums = gather_enum_info cur in
     * List.iter new_print_enum enums;
     * let funcs = gather_func_info cur in
     * List.iter new_print_func funcs;
     * let structs = gather_struct_info cur in
     * List.iter new_print_struct structs; *)

    generate_bindings cur;
    
    (* print_info cur; *)
    if(!cppmode) then
      begin
        let ch = Oclang.Cursor.children cur in
        (* List.iter print_info ch *)
        ()
      end
      
