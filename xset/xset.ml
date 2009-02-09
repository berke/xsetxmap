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

TYPE_CONV_PATH "Xset"

open Sexplib
open Sexp
open Conv
open Xorder

(* $Id: set.ml,v 1.19 2004-11-25 00:04:15 doligez Exp $ *)

(* Sets over ordered types *)

type 'elt cp = 'elt -> 'elt -> int

type 'elt t_inner = Empty | Node of 'elt t_inner * 'elt * 'elt t_inner * int

type ('elt,'order) t = 'elt t_inner

let rec isomap f = function
| Empty -> Empty
| Node(s1, x, s2, n) -> Node(isomap f s1, f x, isomap f s2, n)

(* Sets are represented by balanced binary trees (the heights of the
   children differ by at most 2 *)

let height = function
    Empty -> 0
  | Node(_, _, _, h) -> h

(* Creates a new node with left son l, value v and right son r.
   We must have all elements of l < v < all elements of r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced and | height l - height r | <= 3.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Set.bal"
    | Node(ll, lv, lr, _) ->
        if height ll >= height lr then
          create ll lv (create lr v r)
        else begin
          match lr with
            Empty -> invalid_arg "Set.bal"
          | Node(lrl, lrv, lrr, _)->
              create (create ll lv lrl) lrv (create lrr v r)
        end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Set.bal"
    | Node(rl, rv, rr, _) ->
        if height rr >= height rl then
          create (create l v rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Set.bal"
          | Node(rll, rlv, rlr, _) ->
              create (create l v rll) rlv (create rlr rv rr)
        end
  end else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(* Insertion of one element *)

let rec add cp x = function
    Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
      let c = comparator cp x v in
      if c = 0 then t else
      if c < 0 then bal (add cp x l) v r else bal l v (add cp x r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join cp l v r =
  match (l, r) with
    (Empty, _) -> add cp v r
  | (_, Empty) -> add cp v l
  | (Node(ll, lv, lr, lh), Node(rl, rv, rr, rh)) ->
      if lh > rh + 2 then bal ll lv (join cp lr v r) else
      if rh > lh + 2 then bal (join cp l v rl) rv rr else
      create l v r

(* Smallest and greatest element of a set *)

let rec min_elt = function
    Empty -> raise Not_found
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> min_elt l

let rec max_elt = function
    Empty -> raise Not_found
  | Node(l, v, Empty, _) -> v
  | Node(l, v, r, _) -> max_elt r

(* Remove the smallest element of the given set *)

let rec remove_min_elt = function
    Empty -> invalid_arg "Set.remove_min_elt"
  | Node(Empty, v, r, _) -> r
  | Node(l, v, r, _) -> bal (remove_min_elt l) v r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assume | height l - height r | <= 2. *)

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> bal t1 (min_elt t2) (remove_min_elt t2)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat cp t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) -> join cp t1 (min_elt t2) (remove_min_elt t2)

(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec split cp x = function
    Empty ->
      (Empty, false, Empty)
  | Node(l, v, r, _) ->
      let c = comparator cp x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split cp x l in (ll, pres, join cp rl v r)
      else
        let (lr, pres, rr) = split cp x r in (join cp l v lr, pres, rr)

(* Implementation of the set operations *)

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let is_singleton = function Node(Empty, _, Empty, _) -> true | _ -> false

let rec mem cp x = function
    Empty -> false
  | Node(l, v, r, _) ->
      let c = comparator cp x v in
      c = 0 || mem cp x (if c < 0 then l else r)

let singleton x = Node(Empty, x, Empty, 1)

let rec remove cp x = function
    Empty -> Empty
  | Node(l, v, r, _) ->
      let c = comparator cp x v in
      if c = 0 then merge l r else
      if c < 0 then bal (remove cp x l) v r else bal l v (remove cp x r)

let rec union cp s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then add cp v2 s1 else begin
          let (l2, _, r2) = split cp v1 s2 in
          join cp (union cp l1 l2) v1 (union cp r1 r2)
        end
      else
        if h1 = 1 then add cp v1 s2 else begin
          let (l1, _, r1) = split cp v2 s1 in
          join cp (union cp l1 l2) v2 (union cp r1 r2)
        end

let rec inter cp s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
      match split cp v1 t2 with
        (l2, false, r2) ->
          concat cp (inter cp l1 l2) (inter cp r1 r2)
      | (l2, true, r2) ->
          join cp (inter cp l1 l2) v1 (inter cp r1 r2)

let rec diff cp s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
      match split cp v1 t2 with
        (l2, false, r2) ->
          join cp (diff cp l1 l2) v1 (diff cp r1 r2)
      | (l2, true, r2) ->
          concat cp (diff cp l1 l2) (diff cp r1 r2)

type 'elt enumeration = End | More of 'elt * 'elt t_inner * 'elt enumeration

let rec cons_enum s e =
  match s with
    Empty -> e
  | Node(l, v, r, _) -> cons_enum l (More(v, r, e))

let rec compare_aux cp e1 e2 =
    match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
      let c = comparator cp v1 v2 in
      if c <> 0
      then c
      else compare_aux cp (cons_enum r1 e1) (cons_enum r2 e2)

let compare cp s1 s2 =
  compare_aux cp (cons_enum s1 End) (cons_enum s2 End)

let equal cp s1 s2 =
  compare cp s1 s2 = 0

let rec subset cp s1 s2 =
  match (s1, s2) with
    Empty, _ ->
      true
  | _, Empty ->
      false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
      let c = comparator cp v1 v2 in
      if c = 0 then
        subset cp l1 l2 && subset cp r1 r2
      else if c < 0 then
        subset cp (Node (l1, v1, Empty, 0)) l2 && subset cp r1 t2
      else
        subset cp (Node (Empty, v1, r1, 0)) r2 && subset cp l1 t2

let rec iter f = function
    Empty -> ()
  | Node(l, v, r, _) -> iter f l; f v; iter f r

let rec fold f s accu =
  match s with
    Empty -> accu
  | Node(l, v, r, _) -> fold f r (f v (fold f l accu))

let rec for_all p = function
    Empty -> true
  | Node(l, v, r, _) -> p v && for_all p l && for_all p r

let rec exists p = function
    Empty -> false
  | Node(l, v, r, _) -> p v || exists p l || exists p r

let filter cp p s =
  let rec filt accu = function
    | Empty -> accu
    | Node(l, v, r, _) ->
        filt (filt (if p v then add cp v accu else accu) l) r in
  filt Empty s

let partition cp p s =
  let rec part (t, f as accu) = function
    | Empty -> accu
    | Node(l, v, r, _) ->
        part (part (if p v then (add cp v t, f) else (t, add cp v f)) l) r in
  part (Empty, Empty) s

let rec cardinal = function
    Empty -> 0
  | Node(l, v, r, _) -> cardinal l + 1 + cardinal r

let rec elements_aux accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

let elements s =
  elements_aux [] s

let choose = min_elt

let t_of_sexp cp elt_of_sexp = function
  | List l ->
      List.fold_left
        (fun set x ->
          add cp (elt_of_sexp x) set
        )
        empty
        l
  | Atom _ as sexp -> of_sexp_error "Xset.t_of_sexp: list needed" sexp

let sexp_of_t sexp_of_elt set =
  List(
    fold
      (fun e result ->
        (sexp_of_elt e) :: result
      )
      set
      []
  )

module type GS =
  sig
    type 'elt t
    val empty        : 'elt t
    val is_empty     : 'elt t -> bool
    val is_singleton : 'elt t -> bool
    val mem          : 'elt -> 'elt t -> bool
    val add          : 'elt -> 'elt t -> 'elt t
    val singleton    : 'elt -> 'elt t
    val remove       : 'elt -> 'elt t -> 'elt t
    val union        : 'elt t -> 'elt t -> 'elt t
    val inter        : 'elt t -> 'elt t -> 'elt t
    val diff         : 'elt t -> 'elt t -> 'elt t
    val compare      : 'elt t -> 'elt t -> int
    val equal        : 'elt t -> 'elt t -> bool
    val subset       : 'elt t -> 'elt t -> bool
    val iter         : ('elt -> unit) -> 'elt t -> unit
    val fold         : ('elt -> 'a -> 'a) -> 'elt t -> 'a -> 'a
    val for_all      : ('elt -> bool) -> 'elt t -> bool
    val exists       : ('elt -> bool) -> 'elt t -> bool
    val filter       : ('elt -> bool) -> 'elt t -> 'elt t
    val partition    : ('elt -> bool) -> 'elt t -> 'elt t * 'elt t
    val cardinal     : 'elt t -> int
    val elements     : 'elt t -> 'elt list
    val min_elt      : 'elt t -> 'elt
    val max_elt      : 'elt t -> 'elt
    val choose       : 'elt t -> 'elt
    val split        : 'elt -> 'elt t -> 'elt t * bool * 'elt t
    val t_of_sexp    : (Sexplib.Sexp.t -> 'elt) -> Sexplib.Sexp.t -> 'elt t
    val sexp_of_t    : ('elt -> Sexplib.Sexp.t) -> 'elt t -> Sexplib.Sexp.t
  end

module type GSET =
  sig
    type ('elt, 'order) u = ('elt, 'order) t
    type 'elt tt = ('elt, 'elt cmpo) u
    include GS with type 'elt t = 'elt tt
  end

module type ORDERED =
  sig
    type t with sexp
    val compare : t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty        : t
    val is_empty     : t -> bool
    val is_singleton : t -> bool
    val mem          : elt -> t -> bool
    val add          : elt -> t -> t
    val singleton    : elt -> t
    val remove       : elt -> t -> t
    val union        : t -> t -> t
    val inter        : t -> t -> t
    val diff         : t -> t -> t
    val compare      : t -> t -> int
    val equal        : t -> t -> bool
    val subset       : t -> t -> bool
    val iter         : (elt -> unit) -> t -> unit
    val fold         : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all      : (elt -> bool) -> t -> bool
    val exists       : (elt -> bool) -> t -> bool
    val filter       : (elt -> bool) -> t -> t
    val partition    : (elt -> bool) -> t -> t * t
    val cardinal     : t -> int
    val elements     : t -> elt list
    val min_elt      : t -> elt
    val max_elt      : t -> elt
    val choose       : t -> elt
    val split        : elt -> t -> t * bool * t
    val t_of_sexp    : Sexplib.Sexp.t -> t
    val sexp_of_t    : t -> Sexplib.Sexp.t
  end

module Make(O : ORDERED) =
  struct
    type elt = O.t
    type t = O.t t_inner

    let cmp = Xorder.order () O.compare

    let empty          = empty
    let is_empty       = is_empty
    let is_singleton   = is_singleton  
    let mem x          = mem cmp x
    let add x          = add cmp x
    let singleton      = singleton
    let remove x       = remove cmp x
    let union x        = union cmp x
    let inter x        = inter cmp x
    let diff x         = diff cmp x
    let compare x      = compare cmp x
    let equal x        = equal cmp x
    let subset x       = subset cmp x
    let iter           = iter
    let fold           = fold
    let for_all        = for_all
    let exists         = exists
    let filter x       = filter cmp x
    let partition x    = partition cmp x
    let cardinal       = cardinal
    let elements       = elements
    let min_elt        = min_elt
    let max_elt        = max_elt
    let choose         = choose
    let split x        = split cmp x
    let t_of_sexp x    = t_of_sexp cmp O.t_of_sexp x
    let sexp_of_t x    = sexp_of_t O.sexp_of_t x
  end

module Gset =
  struct
    type ('elt, 'order) u = ('elt, 'order) t
    type 'elt tt = ('elt, 'elt cmpo) u
    type 'elt t = 'elt tt

    let empty          = empty
    let is_empty       = is_empty
    let is_singleton   = is_singleton  
    let mem x          = mem cmp x
    let add x          = add cmp x
    let singleton      = singleton
    let remove x       = remove cmp x
    let union x        = union cmp x
    let inter x        = inter cmp x
    let diff x         = diff cmp x
    let compare x      = compare cmp x
    let equal x        = equal cmp x
    let subset x       = subset cmp x
    let iter           = iter
    let fold           = fold
    let for_all        = for_all
    let exists         = exists
    let filter x       = filter cmp x
    let partition x    = partition cmp x
    let cardinal       = cardinal
    let elements       = elements
    let min_elt        = min_elt
    let max_elt        = max_elt
    let choose         = choose
    let split x        = split cmp x
    let t_of_sexp x    = t_of_sexp cmp x
    let sexp_of_t x    = sexp_of_t x
  end

module Gen =
  struct
    module Gset = Gset
  end
