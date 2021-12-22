open Sexplib.Conv

type module_spec = {
  name: string;
  impl: string option;
  intf: string option;
  cmt: string option;
  cmti: string option;
} [@@deriving sexp, show]

type executable = {
  names: string list;
  requires: string list;
  modules: module_spec list;
}  [@@sexp.allow_extra_fields] [@@deriving sexp, show]

type library = {
  name: string;
  uid: string;
  local: bool;
  requires: string list;
  source_dir: string;
  include_dirs: string list;
  modules: module_spec list;
}  [@@sexp.allow_extra_fields] [@@deriving sexp, show]

  

type t =
  | Executables of executable
  | Library of library [@@deriving show]

let t_of_sexp (sexp: Ppx_sexp_conv_lib.Sexp.t) =
  match sexp with
  | Ppx_sexp_conv_lib.Sexp.Atom _ -> invalid_arg "invalid dune sexp format"
  | Ppx_sexp_conv_lib.Sexp.List [] -> failwith "dune project has not been built"
  | Ppx_sexp_conv_lib.Sexp.List ls ->
    List.map (function 
        | Ppx_sexp_conv_lib.Sexp.(List [(Atom "executables"); rest]) ->
          Executables (executable_of_sexp rest)
        | Ppx_sexp_conv_lib.Sexp.(List [(Atom "library"); rest]) ->
          Library (library_of_sexp rest)
        | _ -> invalid_arg "invalid dune sexp format"
      ) ls

