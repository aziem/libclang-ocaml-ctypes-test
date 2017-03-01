let () =
  let s = Oclang.Util.version () in
  Printf.printf "Hello, clang version is %s\n" s;
  let idex = Oclang.Index.create_index false false in
  let tu = Oclang.TranslationIndex.create_translation_unit_from_source idex "/home/aziem/dbm.c" [] in
  Printf.printf "TU: %s\n" (Oclang.TranslationIndex.get_tu_spelling tu); flush stdout;
  let cur = Oclang.Cursor.cursor_of_translation_unit tu in
  let sz = Oclang.Cursor.num_children cur in
  Printf.printf "SIZE: %d\n" sz; flush stdout;
  Printf.printf "HI %s\n" (Oclang.Cursor.displayname cur); flush stdout; 
  let c = Oclang.Cursor.children cur in
  Printf.printf "Size of c: %d\n" (List.length c); flush stdout;
  List.iter (fun cur -> Printf.printf "%s\n" (Oclang.Cursor.displayname cur)) c;
   Printf.printf "IS NULL %d\n" (Oclang.Cursor.cursor_is_null cur); flush stdout; 
()
