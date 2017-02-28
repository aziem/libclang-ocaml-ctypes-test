open Ctypes

module Bindings (F : Cstubs.FOREIGN) =
struct
  module T = Ffi_types.Types(Ffi_generated_types)

  open F

  type cx_string
  let cx_string : cx_string structure typ = structure "_CXString"
  let cx_string = typedef cx_string "CXString"
  let cx_string_data = field cx_string "data" (ptr void)
  let cx_string_private_flags = field cx_string "private_flags" uint
  let () = seal cx_string

  let getcstring_ = foreign "clang_getCString" (cx_string @-> returning string)

  let disposecstring_ = foreign "clang_disposeString" (cx_string @-> returning void)
                  
  let version_ = foreign "clang_getClangVersion" (void @-> returning cx_string)

  let toggle_crash_recovery_ = foreign "clang_toggleCrashRecovery" (bool @-> returning void)

  type cx_type
  let cx_type : cx_type structure typ = structure "_CXType"
  let cx_type = typedef cx_type "CXType"
  let kind = field cx_type "kind" T.kind
  let data = field cx_type "data" (array 2 (ptr void))
  let () = seal cx_type

  let get_type_spelling = foreign "clang_getTypeSpelling" (cx_type @-> returning cx_string)

  type cx_translation_unit = unit ptr
  let cx_translation_unit : cx_translation_unit typ = ptr void


  type cx_index = unit ptr
  let cx_index : cx_index typ = (ptr void)

  let create_index_ = foreign "clang_createIndex" (int @-> int @-> returning cx_index)
                        
  let create_translation_unit_from_source_ = foreign "clang_createTranslationUnitFromSourceFile" (cx_index @-> string @-> int @-> string @-> int @-> ptr void @-> returning cx_translation_unit)

  let get_tu_spelling = foreign "clang_getTranslationUnitSpelling" (cx_translation_unit @-> returning cx_string)

  type cx_cursor
  let cx_cursor : cx_cursor structure typ = structure "_CXCursor"
  let cx_cursor = typedef cx_cursor "CXCursor" 
  let kind = field cx_cursor "kind" T.cursor_kind
  let xdata = field cx_cursor "xdata" int
  let data = field cx_cursor "data" (array 3 (ptr void))
  let () = seal cx_cursor

  let cursor_of_translation_unit_ = foreign "clang_getTranslationUnitCursor" (cx_translation_unit @-> returning cx_cursor)

  type cx_client_data = unit ptr
  let cx_client_data : cx_client_data typ = ptr void 

  let coerce t fn = coerce (Foreign.funptr t) (static_funptr t) fn
                                  
  let cx_cursor_visitor = Ctypes.(cx_cursor @-> cx_cursor @-> cx_client_data @-> returning T.cx_child_visit_result)
  let visit_children_ = foreign "clang_visitChildren" (cx_cursor @-> (Ctypes.static_funptr cx_cursor_visitor) @-> cx_client_data @-> returning uint)

  let get_cursor_spelling = foreign "clang_getCursorSpelling" (cx_cursor @-> returning cx_string)

  let get_display_name = foreign "clang_getCursorDisplayName" (cx_cursor @-> returning cx_string)

  let cursor_is_null = foreign "clang_Cursor_isNull" (cx_cursor @-> returning int)
                        
end
