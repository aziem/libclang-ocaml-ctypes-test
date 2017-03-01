open Ctypes

module Bindings (T : Cstubs_structs.TYPE with type 'a typ = 'a typ)(F : Cstubs.FOREIGN) =
struct
  (* module T = Ffi_types.Types(Ffi_generated_types) *)

  module E = Ffi_types.Types(T)
  open F

  type cx_string = E.cx_string
  type cx_type = E.cx_type
  type cx_cursor = E.cx_cursor
  let cx_cursor = E.cx_cursor
  type cx_client_data = E.cx_client_data
  let cx_client_data = E.cx_client_data
                   
  let getcstring_ = F.foreign "clang_getCString" (E.cx_string @-> returning T.string)

  let disposecstring_ = F.foreign "clang_disposeString" (E.cx_string @-> returning T.void)
                  
  let version_ = F.foreign "clang_getClangVersion" (void @-> returning E.cx_string)

  let toggle_crash_recovery_ = F.foreign "clang_toggleCrashRecovery" (bool @-> returning T.void)

  let get_type_spelling = foreign "clang_getTypeSpelling" (E.cx_type @-> returning E.cx_string)

  type cx_translation_unit = E.cx_translation_unit
  let cx_translation_unit = E.cx_translation_unit

  type cx_index = E.cx_index
  let cx_index = E.cx_index

  let create_index_ = F.foreign "clang_createIndex" (int @-> int @-> returning cx_index)
                        
  let create_translation_unit_from_source_ = F.foreign "clang_createTranslationUnitFromSourceFile" (cx_index @-> string @-> int @-> string @-> int @-> ptr void @-> returning cx_translation_unit)

  let get_tu_spelling = F.foreign "clang_getTranslationUnitSpelling" (cx_translation_unit @-> returning E.cx_string)

  let cursor_of_translation_unit_ = F.foreign "clang_getTranslationUnitCursor" (cx_translation_unit @-> returning E.cx_cursor)

  let visitor_type = Ctypes.static_funptr T.(E.cx_cursor @-> E.cx_cursor @-> E.cx_client_data @-> returning E.cx_child_visit_result)
  let visitor_typep = T.typedef (visitor_type) "CXCursorVisitor"
                                  
  let visit_children_ = F.foreign "clang_visitChildren" (E.cx_cursor @-> visitor_typep @-> E.cx_client_data @-> returning uint)

  let get_cursor_spelling = F.foreign "clang_getCursorSpelling" (E.cx_cursor @-> returning E.cx_string)

  let get_display_name = F.foreign "clang_getCursorDisplayName" (E.cx_cursor @-> returning E.cx_string)

  let cursor_is_null = F.foreign "clang_Cursor_isNull" (E.cx_cursor @-> returning int)


  let cx_cursor_visitor = T.(E.cx_cursor @-> E.cx_cursor @-> cx_client_data @-> returning E.cx_child_visit_result) 

                       
end
