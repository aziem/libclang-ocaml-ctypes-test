let _ =
  let prefix = "clang_stub" in
  let generate_ml, generate_c = ref false, ref false in
  Arg.(parse [ ("-ml", Set generate_ml, "Generate ML");
               ("-c", Set generate_c, "Generate C") ])
     (fun _ -> failwith "unexpected anonymous argument")
     "stubgen [-ml|-c]";
  match !generate_ml, !generate_c with
  | false, false
  | true, true ->
    failwith "Exactly one of -ml and -c must be specified"
  | true, false ->
    Cstubs.write_ml Format.std_formatter ~prefix (module Ffi_bindings.Bindings)
  | false, true ->
     print_endline ("#include <CXErrorCode.h>");
     print_endline ("#include <CXString.h>");
     print_endline ("#include <Index.h>");
     Cstubs.write_c Format.std_formatter ~prefix (module Ffi_bindings.Bindings)
