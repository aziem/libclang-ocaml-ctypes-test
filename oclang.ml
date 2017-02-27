open Ctypes
open Foreign

module B = Ffi_bindings.Bindings(Ffi_generated)
module Types = B.T

let bool_val b =
  if b then 1 else 0

             
module Util =
  struct 
             
    let version () =
      B.getcstring_ (B.version_ ())
      
    let toggle_crash_recovery flag =
      B.toggle_crash_recovery_ flag
  end


module Type =
  struct
    type t = B.cx_type
    type kind = Types.kind
    type calling_conv = Types.calling_conv
    type layout_error = Types.layout_error

    let name t =
      B.get_type_spelling t |> B.getcstring_

    let kind t =
      getf t B.kind
  end


module Index =
  struct
    type t = B.cx_index

    let create_index exclude_decls_from_pch display_diagnostics =
      B.create_index_ (bool_val exclude_decls_from_pch) (bool_val display_diagnostics)

  end


module TranslationIndex =
  struct
    type t = B.cx_translation_unit
    type options = Types.translation_unit_options

    let create_translation_unit_from_source index file compiler_options =
      let n = List.length compiler_options in
      let s = String.concat "," compiler_options in
      B.create_translation_unit_from_source_ index file n s 0 null 
  end

module Cursor =
  struct
    let kind = Types.cursor_kind
    let cursor_of_translation_unit tu = B.cursor_of_translation_unit_ tu
  end
  