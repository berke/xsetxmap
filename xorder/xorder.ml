(* Xorder *)

type ('a,'b) order = ('a -> 'a -> int)

type cmp

type 'a cmpo = ('a, cmp) order

let cmp = compare

let order n o = o

let comparator o = o
