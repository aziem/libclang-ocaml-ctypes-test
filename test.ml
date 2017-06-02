module Oclang = Ocamlclang


let print_structs cur =
  let print_struct_name cur =
    let s = Oclang.Cursor.name cur in
    Printf.printf "let %s : %s Ctypes.structure T.typ = T.Structure \"%s\"\n" (String.lowercase_ascii s) (String.lowercase_ascii s) s
  in
  let print_field_name structname cur =
    let s = Oclang.Cursor.name cur in
    let ty = Oclang.Type.of_cursor cur in
    Printf.printf "let %s = T.field %s \"%s\" (%s)\n" (String.lowercase_ascii s) structname (String.lowercase_ascii s) (Oclang.Type.name ty)
  in

  let field_visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.FieldDecl -> print_field_name (Oclang.Cursor.name cur) cur
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in
  
  let visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.StructDecl ->
        Printf.printf "type %s\n" (String.lowercase_ascii (Oclang.Cursor.name cur));
        print_struct_name cur;
        Oclang.Cursor.visit cur field_visitor ()
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in
  Oclang.Cursor.visit cur visitor ()

let print_enums cur =
  let print_enum cur =
    Printf.printf "ENUM: %s\n" (Oclang.Cursor.name cur);
  in

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
  
  let visitor cur parent data =
    let () = match Oclang.Cursor.get_kind cur with
      | Oclang.Cursor.EnumDecl ->
        let s = Oclang.Cursor.name cur in
        Printf.printf "type %s = \n" (String.lowercase_ascii s);
        Oclang.Cursor.visit cur constant_visitor ();
        Printf.printf "let %s = T.enum \"%s\" [ \n" (String.lowercase_ascii s) s;
        Oclang.Cursor.visit cur constant_visitor2 ();
        Printf.printf "]\n";
        ()
          
      | _ -> ()
    in
    Oclang.Cursor.Continue, ()
  in
  Oclang.Cursor.visit cur visitor ()



let () =
  let s = Oclang.Util.version () in
  Printf.printf "Hello, clang version is %s\n" s;
  let idex = Oclang.Index.create_index false false in
  let tu = Oclang.TranslationIndex.create_translation_unit_from_source idex "header.h" [] in
  Printf.printf "TU: %s\n" (Oclang.TranslationIndex.get_tu_spelling tu); flush stdout;
  let cur = Oclang.Cursor.cursor_of_translation_unit tu in
  print_structs cur;
  print_enums cur;
