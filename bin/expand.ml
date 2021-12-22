[@@@warning "-27"]
open Core
open Dune_expand_lib

let mangle_name name = String.split ~on:'.' name |> String.concat ~sep:"__"
let try_unmangle_name name =
  let un_dashify name =
    String.Search_pattern.(replace_all (create "__") ~with_:"." ~in_:name) in
  match String.Search_pattern.(index (create "Dune__exe__") ~in_:name) with
  | None -> un_dashify name
  | Some _ ->
    let name = String.Search_pattern.(replace_first (create "Dune__exe__") ~in_:name ~with_:"") in
    "Dune__exe." ^ un_dashify name

let setup_expand_context fn =
  Navigation.change_to_dune_root ();
  let input = Unix.open_process_in "dune describe" in
  let sexp = Sexplib.Sexp.input_sexp input in
  let hashtbl = Hashtbl.create (module String) in
  (* accumulate all modules (lazily) *)
  List.iter ~f:(Ast_collector.iter_spec hashtbl) (Dune_spec.t_of_sexp sexp);
  fn hashtbl

let expand module_ =
  let print_ast mod_ ast =
    print_endline @@ "module " ^ (try_unmangle_name mod_) ^ ":";
    Format.printf "%a\n;;\n%!" Pprintast.structure ast in
  setup_expand_context @@ fun hashtbl ->
  match module_ with
  | None ->
    Hashtbl.iteri hashtbl ~f:(fun ~key:mod_ ~data:(_, lazy ast) ->
      print_ast mod_ ast
    )
  | Some module_ ->
    let raw_module_ = mangle_name module_ in
    match Hashtbl.find hashtbl raw_module_ with
    | None -> failwith ("module " ^ module_ ^ " not found in project")
    | Some (_, lazy ast) ->
      print_ast raw_module_ ast

let list_modules () =
  setup_expand_context @@ fun hashtbl ->
  List.iter (Hashtbl.keys hashtbl) ~f:(fun mod_ ->
    print_endline (try_unmangle_name mod_)
  )

let main list_ module_ =
  if list_
  then list_modules ()
  else expand module_

let () =
  let open Cmdliner in
  let main_command =
    let list_ =
      let info' =
        Arg.info
          ~doc:"Lists all discovered modules in the current dune project."
          ["l"; "list"] in
      Arg.(value @@ flag info') in
    let module_ =
      let info' =
        Arg.info
          ~doc:"(optional) OCaml module (path) to expand. If not \
                provided, dune-expand expands all modules in the \
                project."
          ~docv:"MODULE" ["m"; "module"] in
      Arg.(value @@ opt (some string) None info') in
    Term.(pure main $ list_ $ module_) in
  let default_info = Term.info
                  ~doc:"Pretty prints the expanded output of OCaml \
                        source files in a dune-based OCaml project."
                  ~version:"%%VERSION%%"
                  ~envs:[
                    Term.env_info ~doc:"Integer representing the maximum number of \
                                        directories to search through to find the \
                                        dune-project root before giving up." "DUNE_EXPAND_SEARCH_DEPTH"
                  ] "dune-expand" in
  Term.(exit @@ eval ~catch:true (main_command, default_info))
