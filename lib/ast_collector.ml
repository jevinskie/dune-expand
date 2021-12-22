open Core

type module_summary = {
  source_path: string;
  ast: Parsetree.structure Lazy.t
}

let file_exists file = (try ignore @@ Unix.stat file; true with Unix.Unix_error _ -> false)

let read_ast path =
  In_channel.with_file path ~f:(fun in_channel -> 
    let buf = Buffer.create Misc.Magic_number.magic_length in
    ignore @@ In_channel.input_buffer in_channel ~len:Misc.Magic_number.magic_length buf;
    let magic = Buffer.contents buf in
    match Misc.Magic_number.parse magic with
    | Ok _ -> Pparse.read_ast Structure path
    | Error _ ->
      In_channel.seek in_channel 0L;
      let lexbuf = Lexing.from_channel in_channel in
      Parser.implementation Lexer.token lexbuf
  )

let iter_module_spec map spec =
  match spec.Dune_spec.cmt with
  | Some file when file_exists file ->
    let cmt_file = Cmt_format.read_cmt file in
    let module_ = cmt_file.cmt_modname in
    let path =
      Option.map
        ~f:(fun source_file -> cmt_file.cmt_builddir ^ "/" ^ source_file)
        cmt_file.cmt_sourcefile in
    begin match path with
    | Some path when file_exists path ->
      ignore @@ Hashtbl.add map ~key:module_ ~data:(path, lazy (read_ast path))
    | _ -> ()
    end
  | _ -> ()

let iter_executable map ({ modules; _ }: Dune_spec.executable) =
  List.iter ~f:(iter_module_spec map) modules

let iter_library map ({ modules; _ }: Dune_spec.library) =
  List.iter ~f:(iter_module_spec map) modules

let iter_spec map (sexp: Dune_spec.t) =
  match sexp with
  | Dune_spec.Executables ls ->
    iter_executable map ls
  | Dune_spec.Library ls ->
    iter_library map ls
