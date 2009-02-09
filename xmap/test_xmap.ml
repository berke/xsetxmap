(* Test *)

open Sexplib
open Xmap.Gen

let _ =
  let x = ref Xmap.empty in
  for i = 1 to 100 do
    let n = Random.int 500 in
    x := Xmap.add Xorder.cmp n (Printf.sprintf "0x%08x" n) !x
  done;
  Xmap.iter
    (fun k v ->
      Printf.printf "%d %s\n" k v
    )
    !x

module Foo = struct
  module U : sig type t val t : t end = struct type t = () let t = () end

  type toto

  let gcmp () = Xorder.order U.t (Gmap.compare compare)

  let bar = Gmap.add "toto" 33 (Gmap.add "titi" 66 Gmap.empty)
  let baz = Gmap.add 33.0 33 (Gmap.add 66.0 66 Gmap.empty)

  let foo1 = Xmap.add (gcmp ()) bar 10 Xmap.empty
  (*let foo2 = Xmap.add Xorder.cmp bar 10 foo1*)

  let foo2 = Xmap.add (gcmp ()) baz 10 Xmap.empty

end

(* This should be rejected *)

let reverse_cmp x y = compare y x

module U : sig type t val t : t end = struct type t = () let t = () end

let rev_int : (int, U.t) Xorder.order = Xorder.order U.t reverse_cmp

let _ =
  let x = ref Xmap.empty in
  for i = 1 to 100 do
    let n = Random.int 500 in
    x := Xmap.add Xorder.cmp n (Printf.sprintf "0x%08x" n) !x
  done;
  (*x := Xmap.add rev_int 55 "foo" !x;*)
  Xmap.iter
    (fun k v ->
      Printf.printf "%d %s\n" k v
    )
    !x

let _ =
  let x = ref Gmap.empty in
  for i = 1 to 100 do
    let n = Random.int 500 in
    x := Gmap.add n (Printf.sprintf "0x%08x" n) !x
  done;
  x := Xmap.add Xorder.cmp 55 "foo" !x;
  Gmap.iter
    (fun k v ->
      Printf.printf "%d %s\n" k v
    )
    !x;
  Printf.printf "Sexp:\n%a\n"
    Sexp.output_hum
    (Gmap.sexp_of_t Conv.sexp_of_int Conv.sexp_of_string !x)
