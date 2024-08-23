open Core

let max_search_depth =
  lazy
    (try UnixLabels.getenv "DUNE_EXPAND_SEARCH_DEPTH" |> Int.of_string
     with _ -> 3)

let print_curr_dir () = print_endline @@ Sys.getcwd ()

let files_in_current_directory () =
  let curr_dir = Sys.getcwd () in
  Sys.readdir curr_dir

exception Not_found

let goto_parent_dir () =
  let curr_dir = Sys.getcwd () in
  if String.equal "/" curr_dir then raise Not_found else Sys.chdir ".."

let is_dune_root () =
  let files = files_in_current_directory () in
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
       if i > Lazy.force max_search_depth then raise Not_found;
       goto_parent_dir ()
     with Not_found -> raise (Failure "Not in a dune project"));
    change_to_dune_root (i + 1))

let change_to_dune_root () = change_to_dune_root 0
