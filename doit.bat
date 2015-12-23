ocamlc -c printing.ml
ocamlc -c arithmetic_exercises.ml
ocamlc -c list_exercises.ml
ocamlc -c test_exercises.ml
ocamlc -c test_list_exercises.ml
ocamlc -c test_arithmetic_exercises.ml
ocaml printing.cmo list_exercises.cmo arithmetic_exercises.cmo test_exercises.cmo test_list_exercises.cmo test_arithmetic_exercises.cmo tests.ml
