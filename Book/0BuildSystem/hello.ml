(*
    ocamlc -o hello hello.ml
    ocamlbuild hello
    ./_build/santize.sh -- remove unnecessary files
    ocamlbuild hello
*)
let _ = print_endline "Hello world!"
