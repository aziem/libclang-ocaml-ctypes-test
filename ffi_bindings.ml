open Ctypes

module Bindings (F : Cstubs.FOREIGN) =
struct
  module T = Ffi_types.Types(Ffi_generated_types)


  type cx_string
  let cx_string : cx_string structure typ = structure "CXString"
  let data = field cx_string "data" (ptr void)
  let private_flags = field cx_string "private_flags" uint64_t
  let () = seal cx_string

  let getcstring_ = Foreign.foreign "clang_getCString" (cx_string @-> returning string)
  let version_ = Foreign.foreign "clang_getClangVersion" (void @-> returning cx_string)

  let toggle_crash_recovery_ = Foreign.foreign "clang_toggleCrashRecovery" (bool @-> returning void)

  type cx_type
  let cx_type : cx_type structure typ = structure "CXType"
  let kind = field cx_type "kind" T.kind
  let data = field cx_type "data" (ptr void)
  let () = seal cx_type

  let get_type_spelling = Foreign.foreign "clang_getTypeSpelling" (cx_type @-> returning cx_string)

  type cx_translation_unit = unit ptr
  let cx_translation_unit : cx_translation_unit typ = ptr void


  type cx_index 
  let cx_index = (ptr void)

  let create_index_ = Foreign.foreign "clang_createIndex" (int @-> int @-> returning cx_index)
                        
  let create_translation_unit_from_source_ = Foreign.foreign "clang_createTranslationUnitFromSourceFile"
                                                             (cx_index @-> string @-> int @-> string @-> int @-> ptr void @-> returning cx_translation_unit)

  type cx_cursor
  let cx_cursor : cx_cursor structure typ = structure "CXCursor"
  let kind = field cx_cursor "kind" T.cursor_kind
  let xdata = field cx_cursor "xdata" int
  let data = field cx_cursor "data" (ptr void)
  let () = seal cx_cursor

  let cursor_of_translation_unit_ = Foreign.foreign "clang_getTranslationUnitCursor" (cx_translation_unit @-> returning cx_cursor)
                        
end
