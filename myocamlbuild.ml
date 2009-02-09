(* Myocamlbuild *)

open Ocamlbuild_pack
open Ocamlbuild_plugin
open Command
open Ocaml_specific

let system_lib_dir = "/usr/lib"

let ocamlfind_query pkg =
 let cmd = Printf.sprintf "ocamlfind query %s" (Filename.quote pkg) in
 My_unix.run_and_open cmd (fun ic ->
   Log.dprintf 5 "Getting Ocaml directory from command %s" cmd;
   input_line ic)

let dirs =
  [
    "xorder";
    "xmap";
    "xset";
  ]

let _ = dispatch begin fun x ->
  match x with
  | After_options -> Options.include_dirs := dirs @ !Options.include_dirs
  | After_rules ->
      begin
        let sexplib_dir = ocamlfind_query "sexplib" in
        let type_conv_dir = ocamlfind_query "type-conv" in
        ocaml_lib ~extern:true ~dir:sexplib_dir "sexplib";

        flag
          ["ocaml"; "pp"; "use_sexplib.syntax"] &
          S[A"-I"; A type_conv_dir; A"-I"; A sexplib_dir; A"pa_type_conv.cmo"; A"pa_sexp_conv.cmo"];

        Options.ocamlopt := S[A"ocamlopt.opt";A"-inline";A"1000"];
      end
  | _ -> ()
  end
