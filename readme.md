# opam Dune-expand

Tool to expand PPXs in dune-based projects.

Run:
```
dune-expand
```
Output:
```
module Dune__exe.Expand:
...
;;
...
module Dune_expand_lib.Dune_spec:
...
;;
```

## Fork notice
This was forked to work with newer OCaml versions from [Kiran Gopinathan's original repo](https://gitlab.com/gopiandcode/dune-expand).

## Usage

```
NAME
       dune-expand - Pretty prints the expanded output of OCaml source files
       in a dune-based OCaml project.

SYNOPSIS
       dune-expand [OPTION]... 

OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       -l, --list
           Lists all discovered modules in the current dune project.

       -m MODULE, --module=MODULE
           (optional) OCaml module (path) to expand. If not provided,
           dune-expand expands all modules in the project.

       --version
           Show version information.

ENVIRONMENT
       These environment variables affect the execution of dune-expand:

       DUNE_EXPAND_SEARCH_DEPTH
           Integer representing the maximum number of directories to search
           through to find the dune-project root before giving up.

```
