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

(* $Id: set.mli,v 1.33 2005-07-21 14:52:45 doligez Exp $ *)

(** Sets over ordered types.

   This module implements the set data structure, given a total ordering
   function over the set elements. All operations over sets
   are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and is therefore
   reasonably efficient: insertion and membership take time
   logarithmic in the size of the set, for instance.
*)

open Xorder

type ('elt, 'order) t
(** The type of sets. *)

val empty: ('elt, 'order) t
(** The empty set. *)

val is_empty: ('elt, 'order) t -> bool
(** Test whether a set is empty or not. *)

val is_singleton: ('elt, 'order) t -> bool
(** Test whether a set is a singleton or not (in O(1) time). *)

val mem: ('elt, 'name) order -> 'elt -> ('elt, 'order) t -> bool
(** [mem x s] tests whether [x] belongs to the set [s]. *)

val add: ('elt, 'name) order -> 'elt -> ('elt, 'order) t -> ('elt, 'order) t
(** [add x s] returns a set containing all elements of [s],
   plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

val singleton: 'elt -> ('elt, 'order) t
(** [singleton x] returns the one-element set containing only [x]. *)

val remove: ('elt, 'name) order -> 'elt -> ('elt, 'order) t -> ('elt, 'order) t
(** [remove x s] returns a set containing all elements of [s],
   except [x]. If [x] was not in [s], [s] is returned unchanged. *)

val union: ('elt, 'name) order -> ('elt, 'order) t -> ('elt, 'order) t -> ('elt, 'order) t
(** Set union. *)

val inter: ('elt, 'name) order -> ('elt, 'order) t -> ('elt, 'order) t -> ('elt, 'order) t
(** Set intersection. *)

(** Set difference. *)
val diff: ('elt, 'name) order -> ('elt, 'order) t -> ('elt, 'order) t -> ('elt, 'order) t

val compare: ('elt, 'name) order -> ('elt, 'order) t -> ('elt, 'order) t -> int
(** Total ordering between sets. Can be used as the ordering function
   for doing sets of sets. *)

val equal: ('elt, 'name) order -> ('elt, 'order) t -> ('elt, 'order) t -> bool
(** [equal s1 s2] tests whether the sets [s1] and [s2] are
   equal, that is, contain equal elements. *)

val subset: ('elt, 'name) order -> ('elt, 'order) t -> ('elt, 'order) t -> bool
(** [subset s1 s2] tests whether the set [s1] is a subset of
   the set [s2]. *)

val iter: ('elt -> unit) -> ('elt, 'order) t -> unit
(** [iter f s] applies [f] in turn to all elements of [s].
   The elements of [s] are presented to [f] in increasing order
   with respect to the ordering over the type of the elements. *)

val fold: ('elt -> 'a -> 'a) -> ('elt, 'order) t -> 'a -> 'a
(** [fold f s a] computes [(f xN ... (f x2 (f x1 a))...)],
   where [x1 ... xN] are the elements of [s], in increasing order. *)

val for_all: ('elt -> bool) -> ('elt, 'order) t -> bool
(** [for_all p s] checks if all elements of the set
   satisfy the predicate [p]. *)

val exists: ('elt -> bool) -> ('elt, 'order) t -> bool
(** [exists p s] checks if at least one element of
   the set satisfies the predicate [p]. *)

val filter: ('elt, 'name) order -> ('elt -> bool) -> ('elt, 'order) t -> ('elt, 'order) t
(** [filter p s] returns the set of all elements in [s]
   that satisfy predicate [p]. *)

val partition: ('elt, 'name) order -> ('elt -> bool) -> ('elt, 'order) t -> ('elt, 'order) t * ('elt, 'order) t
(** [partition p s] returns a pair of sets [(s1, s2)], where
   [s1] is the set of all the elements of [s] that satisfy the
   predicate [p], and [s2] is the set of all the elements of
   [s] that do not satisfy [p]. *)

val cardinal: ('elt, 'order) t -> int
(** Return the number of elements of a set. *)

val elements: ('elt, 'order) t -> 'elt list
(** Return the list of all elements of the given set.
   The returned list is sorted in increasing order with respect
   to the ordering [Ord.compare], where [Ord] is the argument
   given to {!Set.Make}. *)

val min_elt: ('elt, 'order) t -> 'elt
(** Return the smallest element of the given set
   (with respect to the [Ord.compare] ordering), or raise
   [Not_found] if the set is empty. *)

val max_elt: ('elt, 'order) t -> 'elt
(** Same as {!Set.S.min_'elt}, but returns the largest element of the
   given set. *)

val choose: ('elt, 'order) t -> 'elt
(** Return one element of the given set, or raise [Not_found] if
   the set is empty. Which element is chosen is unspecified,
   but equal elements will be chosen for equal sets. *)

val split: ('elt, 'name) order -> 'elt -> ('elt, 'order) t -> ('elt, 'order) t * bool * ('elt, 'order) t
(** [split x s] returns a triple [(l, present, r)], where
      [l] is the set of elements of [s] that are
      strictly less than [x];
      [r] is the set of elements of [s] that are
      strictly greater than [x];
      [present] is [false] if [s] contains no element equal to [x],
      or [true] if [s] contains an element equal to [x]. *)

val t_of_sexp : ('elt, 'order) order -> (Sexplib.Sexp.t -> 'elt) -> Sexplib.Sexp.t -> ('elt, 'order) t

val sexp_of_t : ('elt -> Sexplib.Sexp.t) -> ('elt, 'order) t -> Sexplib.Sexp.t

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

module Gen :
  sig
    module Gset : GSET
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

module type ORDERED =
  sig
    type t with sexp
    val compare : t -> t -> int
  end

module Make : functor(O : ORDERED) -> S with type elt = O.t
