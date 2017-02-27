let () =
  print_endline ("#include <CXErrorCode.h>");
  print_endline ("#include <CXString.h>");
  print_endline ("#include <Index.h>");
  Cstubs.Types.write_c Format.std_formatter (module Ffi_types.Types)
