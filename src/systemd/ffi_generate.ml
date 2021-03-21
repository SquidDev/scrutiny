let with_fmt out f =
  CCIO.with_out out @@ fun h ->
  let fmt = Format.formatter_of_out_channel h in
  f fmt; Format.pp_print_flush fmt ()

let pp_headers f =
  Format.fprintf f "#define SD_JOURNAL_SUPPRESS_LOCATION\n";
  [ "systemd/sd-journal.h" ] |> List.iter @@ fun h -> Format.fprintf f "#include <%s>\n" h

let () =
  with_fmt "ffi_generated.ml" (fun f ->
      Cstubs.write_ml f ~prefix:"scr_journal" (module Ffi_bindings.Values));

  with_fmt "ffi_generated.c" (fun f ->
      pp_headers f;
      Cstubs.write_c f ~prefix:"scr_journal" (module Ffi_bindings.Values));

  with_fmt "ffi_generated_types.write.c" (fun f ->
      pp_headers f;
      Cstubs_structs.write_c f (module Ffi_bindings.Constants))
