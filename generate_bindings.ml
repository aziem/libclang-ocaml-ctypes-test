module Oclang = Ocamlclang

let verbose = ref false
let cppmode = ref false
let filename = ref ""

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


let print_func_decl cur =
  let rec print_type cur ty =
    let tyname = Oclang.Type.name ty in
    begin
      match Oclang.Type.kind ty with
      | Oclang.Type.Pointer ->
        let ptee = Oclang.Type.get_pointee_type ty in
        let res =  begin
          (* If its a char pointer, just return string *)
            match Oclang.Type.kind ptee with
            | Oclang.Type.Char_S
            | Oclang.Type.Char_U -> Printf.sprintf "T.string "
            | _ ->  Printf.sprintf "T.ptr (%s) " (print_type cur ptee)
        end
        in
        res
      | Oclang.Type.Void -> Printf.sprintf "T.void "
      | Oclang.Type.UInt -> Printf.sprintf "T.size_t"
      | Oclang.Type.Int -> Printf.sprintf "T.int "
      | Oclang.Type.Bool -> Printf.sprintf "T.bool "
      | Oclang.Type.Char_S 
      | Oclang.Type.Char_U -> Printf.sprintf "T.string "
      | _ -> Printf.sprintf " %s " (to_snake_case tyname)
    end
  in
  
  let parm_visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.ParmDecl ->
        let st = print_type cur (Oclang.Type.of_cursor cur) in
        Printf.printf " %s @-> " st
      | _ -> Printf.printf " %s " (Oclang.Cursor.name cur)
    in
    Oclang.Cursor.Continue, ()
  in
  

  let rec visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.FunctionDecl ->
        let s = Oclang.Cursor.name cur in
        let ty = Oclang.Type.of_cursor cur |> Oclang.Type.get_result_type  in
        Printf.printf "let %s = F.foreign  \"%s\" (" (to_snake_case s) s;
        Oclang.Cursor.visit cur parm_visitor ();
        Printf.printf "returning (%s))\n" (print_type cur ty)
      | Oclang.Cursor.LinkageSpec ->
        Printf.printf "LINKAGE: %s\n" (Oclang.Cursor.name cur); ()
      | Oclang.Cursor.Namespace ->
        Printf.printf "Namespace: %s\n" (Oclang.Cursor.name cur); ()
      | _ -> ();
        Oclang.Cursor.visit cur visitor ()
        
    in
    Oclang.Cursor.Continue, ()
  in
  Oclang.Cursor.visit cur visitor ()



let print_structs cur =
  let print_struct_name cur =
    let s = Oclang.Cursor.name cur in
    Printf.printf "let %s : %s Ctypes.structure T.typ = T.structure \"%s\"\n" (to_snake_case s) (to_snake_case s) s
  in
  let print_field_name structname cur =
    let s = Oclang.Cursor.name cur in
    let ty = Oclang.Type.of_cursor cur in
    Printf.printf "let %s = T.field %s \"%s\" (%s)\n" (structname ^ "_" ^ (String.lowercase_ascii s)) structname s (Oclang.Type.name ty)
  in

  let field_visitor structname cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.FieldDecl -> print_field_name structname cur
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in
  
  let visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.StructDecl ->
        let s = Oclang.Cursor.name cur in
        Printf.printf "type %s\n" (to_snake_case (Oclang.Cursor.name cur));
        print_struct_name cur;
        Oclang.Cursor.visit cur (field_visitor (to_snake_case s)) ()
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in
  Oclang.Cursor.visit cur visitor ()

let print_enums cur =
  let print_enum_constant cur =
    Printf.printf "\t| %s\n" (Oclang.Cursor.name cur);
  in

  let constant_visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.EnumConstantDecl ->
        print_enum_constant cur;
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in

  let constant_visitor2 cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.EnumConstantDecl ->
        let s = Oclang.Cursor.name cur in
        Printf.printf "\t%s, %s;\n" s (String.lowercase_ascii s);
      | _-> ()
    in
    Oclang.Cursor.Continue, ()
  in

  let constant_visitor3 cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.EnumConstantDecl ->
        let s = Oclang.Cursor.name cur in
        Printf.printf "let %s = T.constant \"%s\" T.uint64_t\n" (String.lowercase_ascii s) s
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in
  
  let visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.EnumDecl ->
        let s = Oclang.Cursor.name cur in
        Printf.printf "type %s = \n" (String.lowercase_ascii s);
        Oclang.Cursor.visit cur constant_visitor ();
        Oclang.Cursor.visit cur constant_visitor3 ();
        Printf.printf "let %s = T.enum \"%s\" [ \n" (String.lowercase_ascii s) s;
        Oclang.Cursor.visit cur constant_visitor2 ();
        Printf.printf "]\n";
        ()
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in
  Oclang.Cursor.visit cur visitor ()

let print_info cur = 
  print_structs cur;
  print_enums cur;
  print_func_decl cur


let () =
  let speclist =
    [ ("-cppmode", Arg.Set cppmode, "Enables c++ mode parsing");
      ("-file", Arg.Set_string filename, "File name to parse");]
  in
  let usage_msg = "Pass a C header file to get some OCaml bindings out" in
  Arg.parse speclist print_endline usage_msg;
  let s = Oclang.Util.version () in
  Printf.printf "Clang version is %s\n" s;
  let idex = Oclang.Index.create_index false false in
  let cur =
    Oclang.TranslationIndex.parse_translation_unit ~iscpp:!cppmode idex !filename [] |>
    Oclang.Cursor.cursor_of_translation_unit
  in
  print_info cur;
  if(!cppmode) then
    begin
      let ch = Oclang.Cursor.children cur in
      List.iter print_info ch
    end
