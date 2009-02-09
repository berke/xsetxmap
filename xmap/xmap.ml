(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* Unfunctorized, slightly extended and made Sexplib-compatible by Berke Durak *)

open Sexplib
open Sexp
open Conv
open Xorder

TYPE_CONV_PATH "Xmap"

(* $Id: map.ml,v 1.17 2005-08-13 20:59:37 doligez Exp $ *)

type ('key,'data) t_inner =
    Empty
  | Node of ('key,'data) t_inner * 'key * 'data * ('key,'data) t_inner * int

type ('key,'data,'order) t = ('key,'data) t_inner

let rec isomap fk fv = function
    Empty -> Empty
  | Node(l, x, d, r, n) -> Node(isomap fk fv l, fk x, fv d, isomap fk fv r, n)

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
        if height ll >= height lr then
          create ll lv ld (create lr x d r)
        else begin
          match lr with
            Empty -> invalid_arg "Map.bal"
          | Node(lrl, lrv, lrd, lrr, _)->
              create (create ll lv ld lrl) lrv lrd (create lrr x d r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
        if height rr >= height rl then
          create (create l x d rl) rv rd rr
        else begin
          match rl with
            Empty -> invalid_arg "Map.bal"
          | Node(rll, rlv, rld, rlr, _) ->
              create (create l x d rll) rlv rld (create rlr rv rd rr)
        end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let rec add ck x data = function
    Empty ->
      Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
      let c = comparator ck x v in
      if c = 0 then
        Node(l, x, data, r, h)
      else if c < 0 then
        bal (add ck x data l) v d r
      else
        bal l v d (add ck x data r)

let rec find ck x = function
    Empty ->
      raise Not_found
  | Node(l, v, d, r, _) ->
      let c = comparator ck x v in
      if c = 0 then d
      else find ck x (if c < 0 then l else r)

let findopt ck default x y =
  try
    find ck x y
  with
  | Not_found -> default

let rec mem ck x = function
    Empty ->
      false
  | Node(l, v, d, r, _) ->
      let c = comparator ck x v in
      c = 0 || mem ck x (if c < 0 then l else r)

let rec min_binding = function
    Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> min_binding l

let rec remove_min_binding = function
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
      let (x, d) = min_binding t2 in
      bal t1 x d (remove_min_binding t2)

let rec remove ck x = function
    Empty ->
      Empty
  | Node(l, v, d, r, h) ->
      let c = comparator ck x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        bal (remove ck x l) v d r
      else
        bal l v d (remove ck x r)

let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
      iter f l; f v d; iter f r

let rec reverse_iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
      reverse_iter f r; f v d; reverse_iter f l

let rec map f = function
    Empty               -> Empty
  | Node(l, v, d, r, h) -> Node(map f l, v, f d, map f r, h)

let rec mapi f = function
    Empty               -> Empty
  | Node(l, v, d, r, h) -> Node(mapi f l, v, f v d, mapi f r, h)

let rec fold f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
      fold f r (f v d (fold f l accu))

let rec reverse_fold f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
      reverse_fold f l (f v d (reverse_fold f r accu))

type ('key,'data) enumeration = End | More of 'key * 'data * ('key,'data) t_inner * ('key,'data) enumeration

let rec cons_enum m e =
  match m with
    Empty -> e
  | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

let compare ck cmp m1 m2 =
  let rec compare_aux e1 e2 =
      match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        let c = comparator ck v1 v2 in
        if c <> 0 then c else
        let c = cmp d1 d2 in
        if c <> 0 then c else
        compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in compare_aux (cons_enum m1 End) (cons_enum m2 End)

let equal ck cmp m1 m2 =
  let rec equal_aux e1 e2 =
      match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
        comparator ck v1 v2 = 0 && cmp d1 d2 &&
        equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in equal_aux (cons_enum m1 End) (cons_enum m2 End)

let rec min_key = function
    Empty -> raise Not_found
  | Node(Empty, k, _, _, _) -> k
  | Node(l, _, _, _, _) -> min_key l

let rec max_key = function
    Empty -> raise Not_found
  | Node(_, k, _, Empty, _) -> k
  | Node(_, _, _, r, _) -> max_key r

let choose_key = function
    Empty -> raise Not_found
  | Node(_,k,_,_,_) -> k

let choose ck data = find ck (choose_key data) data

let rec length = function
    Empty -> 0
  | Node(l, _, _, r, _) -> (length l) + (length r) + 1

let pairs data = fold (fun k v l -> (k, v) :: l) data []

let t_of_sexp ck key_of_sexp data_of_sexp = function
  | List l ->
      List.fold_left
        (fun map x ->
          let (k,v) = pair_of_sexp key_of_sexp data_of_sexp x in
          add ck k v map
        )
        empty
        l
  | Atom _ as sexp -> of_sexp_error "Xmap.t_of_sexp: list needed" sexp

let sexp_of_t sexp_of_key sexp_of_data map =
  List(
    fold
      (fun k v result ->
        (sexp_of_pair sexp_of_key sexp_of_data (k,v)) :: result
      )
      map
      []
  )

module type GMAP =
  sig
    type ('key, 'data, 'order) u = ('key, 'data, 'order) t
    type ('key, 'data) t = ('key, 'data, 'key cmpo) u

    val empty        : ('key, 'data) t
    val is_empty     : ('key, 'data) t -> bool
    val add          : 'key -> 'data -> ('key, 'data) t -> ('key, 'data) t
    val find         : 'key -> ('key, 'data) t -> 'data
    val findopt      : 'data -> 'key -> ('key, 'data) t -> 'data
    val remove       : 'key -> ('key, 'data) t -> ('key, 'data) t
    val mem          : 'key -> ('key, 'data) t -> bool
    val iter         : ('key -> 'data -> unit) -> ('key, 'data) t -> unit
    val reverse_iter : ('key -> 'data -> unit) -> ('key, 'data) t -> unit
    val map          : ('data1 -> 'data2) -> ('key, 'data1) t -> ('key, 'data2) t
    val mapi         : ('key -> 'data1 -> 'data2) -> ('key, 'data1) t -> ('key, 'data2) t
    val fold         : ('key -> 'data -> 'b -> 'b) -> ('key, 'data) t -> 'b -> 'b
    val reverse_fold : ('key -> 'data -> 'b -> 'b) -> ('key, 'data) t -> 'b -> 'b
    val compare      : ('data -> 'data -> int) -> ('key, 'data) t -> ('key, 'data) t -> int
    val equal        : ('data -> 'data -> bool) -> ('key, 'data) t -> ('key, 'data) t -> bool
    val min_key      : ('key, 'data) t -> 'key
    val max_key      : ('key, 'data) t -> 'key
    val choose_key   : ('key, 'data) t -> 'key
    val choose       : ('key, 'data) t -> 'data
    val length       : ('key, 'data) t -> int
    val pairs        : ('key, 'data) t -> ('key * 'data) list
    val t_of_sexp    : (Sexplib.Sexp.t -> 'key) -> (Sexplib.Sexp.t -> 'data) -> Sexplib.Sexp.t -> ('key, 'data) t
    val sexp_of_t    : ('key -> Sexplib.Sexp.t) -> ('data -> Sexplib.Sexp.t) -> ('key, 'data) t -> Sexplib.Sexp.t
  end

module Gmap =
  struct
    type ('key, 'data, 'order) u = ('key, 'data, 'order) t
    type ('key, 'data) t = ('key, 'data, 'key cmpo) u

    let empty = empty
    let is_empty = is_empty
    let add x = add cmp x
    let find x = find cmp x
    let findopt x = findopt cmp x
    let remove x = remove cmp x
    let mem x = mem cmp x
    let iter x = iter x
    let reverse_iter x = reverse_iter x
    let map x = map x
    let mapi x = mapi x
    let fold x = fold x
    let reverse_fold x = reverse_fold x
    let compare x = compare cmp x
    let equal x = equal cmp x
    let min_key x = min_key x
    let max_key x = max_key x
    let choose_key x = choose_key x
    let choose x = choose cmp x
    let length x = length x
    let pairs x = pairs x
    let t_of_sexp x = t_of_sexp cmp x
    let sexp_of_t x = sexp_of_t x
  end

module Gen =
  struct
    module Gmap = Gmap
  end

module type ORDERED =
  sig
    type t with sexp
    val compare : t -> t -> int
  end

module type S =
  sig
    type key
    type 'a t

    val empty        : 'data t
    val is_empty     : 'data t -> bool
    val add          : key -> 'data -> 'data t -> 'data t
    val find         : key -> 'data t -> 'data
    val findopt      : 'data -> key -> 'data t -> 'data
    val remove       : key -> 'data t -> 'data t
    val mem          : key -> 'data t -> bool
    val iter         : (key -> 'data -> unit) -> 'data t -> unit
    val reverse_iter : (key -> 'data -> unit) -> 'data t -> unit
    val map          : ('data1 -> 'data2) -> 'data1 t -> 'data2 t
    val mapi         : (key -> 'data1 -> 'data2) -> 'data1 t -> 'data2 t
    val fold         : (key -> 'data -> 'b -> 'b) -> 'data t -> 'b -> 'b
    val reverse_fold : (key -> 'data -> 'b -> 'b) -> 'data t -> 'b -> 'b
    val compare      : ('data -> 'data -> int) -> 'data t -> 'data t -> int
    val equal        : ('data -> 'data -> bool) -> 'data t -> 'data t -> bool
    val min_key      : 'data t -> key
    val max_key      : 'data t -> key
    val choose_key   : 'data t -> key
    val choose       : 'data t -> 'data
    val length       : 'data t -> int
    val pairs        : 'data t -> (key * 'data) list
    val t_of_sexp    : (Sexplib.Sexp.t -> 'data) -> Sexplib.Sexp.t -> 'data t
    val sexp_of_t    : ('data -> Sexplib.Sexp.t) -> 'data t -> Sexplib.Sexp.t
  end

module Make(O : ORDERED) =
  struct
    type key = O.t
    type 'data t = (key, 'data) t_inner
    let cmp = Xorder.order () O.compare

    let empty = empty
    let is_empty = is_empty
    let add x = add cmp x
    let find x = find cmp x
    let findopt x = findopt cmp x
    let remove x = remove cmp x
    let mem x = mem cmp x
    let iter x = iter x
    let reverse_iter x = reverse_iter x
    let map x = map x
    let mapi x = mapi x
    let fold x = fold x
    let reverse_fold x = reverse_fold x
    let compare x = compare cmp x
    let equal x = equal cmp x
    let min_key x = min_key x
    let max_key x = max_key x
    let choose_key x = choose_key x
    let choose x = choose cmp x
    let length x = length x
    let pairs x = pairs x
    let t_of_sexp x = t_of_sexp cmp O.t_of_sexp x
    let sexp_of_t x = sexp_of_t O.sexp_of_t x
  end
