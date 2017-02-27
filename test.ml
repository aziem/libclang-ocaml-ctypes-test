let () =
  let s = Oclang.Util.version () in
  let idex = Oclang.Index.create_index false false in
  let tu = Oclang.TranslationIndex.create_translation_unit_from_source idex "/home/aziem/test.c" [] in
  let cur = Oclang.Cursor.cursor_of_translation_unit tu in
  Printf.printf "Hello, clang version is %s\n" s
