ocamlc -c priority_queue.ml
@if errorlevel 1 goto failure
ocamlc -c printing.ml
@if errorlevel 1 goto failure
ocamlc -c list_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c arithmetic_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c logic_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c binary_tree_exercises.ml
@if errorlevel 1 goto failure

ocamlc -c test_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c test_list_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c test_arithmetic_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c test_logic_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c test_binary_tree_exercises.ml
@if errorlevel 1 goto failure

ocaml printing.cmo binary_tree_exercises.cmo test_exercises.cmo test_binary_tree_exercises.cmo tests.ml
@if errorlevel 1 goto failure
exit
:failure
@echo FAILURE
