ocamlc -c printing.ml
ocamlc -c list_exercises.ml
ocamlc -c test_exercises.ml
ocaml printing.cmo list_exercises.cmo test_exercises.cmo test_list_exercises.ml
