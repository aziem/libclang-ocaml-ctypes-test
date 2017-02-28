let () =
  let s = Oclang.Util.version () in
  Printf.printf "Hello, clang version is %s\n" s;
  let idex = Oclang.Index.create_index false false in
  let tu = Oclang.TranslationIndex.create_translation_unit_from_source idex "/home/aziem/dbm.c" [] in
  Printf.printf "TU: %s\n" (Oclang.TranslationIndex.get_tu_spelling tu); flush stdout;
  let cur = Oclang.Cursor.cursor_of_translation_unit tu in
  Printf.printf "HI %s\n" (Oclang.Cursor.displayname cur); flush stdout; 
   Printf.printf "IS NULL %d\n" (Oclang.Cursor.cursor_is_null cur); flush stdout; 
()
