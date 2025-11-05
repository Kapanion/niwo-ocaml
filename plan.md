# Plan for Setting Up niwo-ocaml Project

## Project Overview
The niwo-ocaml project aims to implement the niwo2 tool in OCaml. The initial functionality will focus on reading a transition system file and confirming its existence by printing out the number of lines. This document outlines the steps to set up the project environment using Dune and implement the basic functionality.

## Steps to Set Up the Environment

1. **Create Project Directory Structure**
   - Ensure the following directory structure is created:
     ```
     niwo-ocaml
     ├── bin
     ├── src
     ├── test
     ├── .gitignore
     ├── dune
     ├── dune-project
     ├── niwo-ocaml.opam
     ├── plan.md
     └── README.md
     ```

2. **Initialize Dune Project**
   - Create a `dune-project` file in the root directory with the following content:
     ```
     (lang dune 3.7)
     (name niwo-ocaml)
     (generate_opam_files true)
     (authors "Your Name")
     (maintainers "Your Email")
     (license MIT)
     (source (github username/niwo-ocaml))
     ```

3. **Set Up Binary Executable**
   - In the `bin` directory, create a `dune` file with the following content:
     ```
     (executable
       (name main)
       (libraries niwo))
     ```
   - Create `main.ml` in the `bin` directory with the following content:
     ```ocaml
     let () =
       if Array.length Sys.argv < 2 then begin
         Printf.eprintf "Usage: %s <transition_system_file>\n" Sys.argv.(0);
         exit 1
       end;
       let filename = Sys.argv.(1) in
       Niwo.process_file filename
     ```

4. **Set Up Library**
   - In the `src` directory, create a `dune` file with the following content:
     ```
     (library
       (name niwo))
     ```
   - Create `niwo.mli` (interface file) in the `src` directory.
   - Create `niwo.ml` in the `src` directory with the `process_file` function that reads the input file and outputs the number of lines in it, including proper error handling.

5. **Set Up Test Suite**
   - In the `test` directory, create a `dune` file with the following content:
     ```
     (test
       (name test_niwo)
       (libraries niwo alcotest))
     ```
   - Create `test_niwo.ml` in the `test` directory with basic tests.

6. **Create OPAM File**
   - Create `niwo-ocaml.opam` with the following content:
     ```
     opam-version: "2.0"
     name: "niwo-ocaml"
     version: "0.1.0"
     synopsis: "NIWO2 implementation in OCaml"
     description: "A project to implement the NIWO2 tool in OCaml."
     depends: [
       "ocaml" {>= "4.14.0"}
       "dune" {>= "3.7"}
       "alcotest" {with-test}
     ]
     ```

7. **Documentation**
   - Update `README.md` with project details, setup instructions, and usage guidelines.

8. **Add .gitignore**
   - Create a `.gitignore` file in the root directory with the following content:
     ```
     _build/
     *.install
     *.merlin
     .ocamlformat
     ```


By following this plan, the niwo-ocaml project will be set up and ready for development.