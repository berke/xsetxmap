(* Xorder *)

type ('key, 'name) order

type cmp

type 'a cmpo = ('a, cmp) order

val cmp : ('a, cmp) order

val order : 'name -> ('key -> 'key -> int) -> ('key, 'name) order

val comparator : ('key, 'name) order -> 'key -> 'key -> int
