open Core
open Sexplib0

let max_search_depth =
  lazy
    (try UnixLabels.getenv "DUNE_EXPAND_SEARCH_DEPTH" |> Int.of_string
     with _ -> 3)

let print_curr_dir () = print_endline @@ Core_unix.getcwd ()

let files_in_current_directory () =
  let curr_dir = Core_unix.opendir (Core_unix.getcwd ()) in
  let rec loop acc =
    match Core_unix.readdir_opt curr_dir with
    | None -> acc
    | Some file -> loop (file :: acc)
  in
  loop []

let goto_parent_dir () =
  let curr_dir = Core_unix.getcwd () in
  if String.equal "/" curr_dir then
    raise (Not_found_s (Sexp.message "no parent dir" []))
  else Core_unix.chdir ".."

let is_dune_root () =
  let files = Array.of_list (files_in_current_directory ()) in
  let dune_project = ref false and _build = ref false in
  Array.iter
    ~f:(function
      | "dune-project" -> dune_project := true
      | "_build" -> _build := true
      | _ -> ())
    files;
  !_build && !dune_project

let rec change_to_dune_root i =
  if is_dune_root () then ()
  else (
    (try
       if i > Lazy.force max_search_depth then
         raise (Not_found_s (Sexp.message "" []));
       goto_parent_dir ()
     with Not_found_s _ -> raise (Failure "Not in a dune project"));
    change_to_dune_root (i + 1))

let change_to_dune_root () = change_to_dune_root 0
