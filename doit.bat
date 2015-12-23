ocamlc -c printing.ml
@if errorlevel 1 goto failure
ocamlc -c list_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c arithmetic_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c test_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c test_list_exercises.ml
@if errorlevel 1 goto failure
ocamlc -c test_arithmetic_exercises.ml
@if errorlevel 1 goto failure
ocaml printing.cmo list_exercises.cmo arithmetic_exercises.cmo test_exercises.cmo test_list_exercises.cmo test_arithmetic_exercises.cmo tests.ml
@if errorlevel 1 goto failure
exit
:failure
@echo FAILURE
