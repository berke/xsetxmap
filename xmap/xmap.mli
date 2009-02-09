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

(* $Id: map.mli,v 1.33 2005-10-25 18:34:07 doligez Exp $ *)

(** Association tables over ordered types.

   This module implements applicative association tables, also known as
   finite maps or dictionaries, given a total ordering function
   over the keys.
   All operations over maps are purely applicative (no side-effects).
   The implementation uses balanced binary trees, and therefore searching
   and insertion take time logarithmic in the size of the map.
*)

open Xorder

type ('key,'data,'order) t
(** The type of maps from type ['a] to type ['b]. *)

(*val isomap : ('key1 -> 'key2) -> ('data1 -> 'data2) -> ('key1, 'data1) t -> ('key2, 'data2) t*)

val empty: ('key, 'data, 'order) t
(** The empty map. *)

val is_empty: ('key, 'data, 'order) t -> bool
(** Test whether a map is empty or not. *)

val add: ('key, 'name) order -> 'key -> 'data -> ('key, 'data, ('key, 'name) order) t -> ('key, 'data, ('key, 'name) order) t
(** [add x y m] returns a map containing the same bindings as
   [m], plus a binding of [x] to [y]. If [x] was already bound
   in [m], its previous binding disappears. *)

val find: ('key, 'name) order -> 'key -> ('key, 'data, ('key, 'name) order) t -> 'data
(** [find x m] returns the current binding of [x] in [m],
   or raises [Not_found] if no such binding exists. *)

val findopt: ('key, 'name) order -> 'data -> 'key -> ('key, 'data, ('key, 'name) order) t -> 'data
(** [findopt x default m] returns the current binding of [x] in [m],
   or [default] if no such binding exists. *)

val remove: ('key, 'name) order -> 'key -> ('key, 'data, ('key, 'name) order) t -> ('key, 'data, ('key, 'name) order) t
(** [remove x m] returns a map containing the same bindings as
   [m], except for [x] which is unbound in the returned map. *)

val mem: ('key, 'name) order -> 'key -> ('key, 'data, ('key, 'name) order) t -> bool
(** [mem x m] returns [true] if [m] contains a binding for [x],
   and [false] otherwise. *)

val iter: ('key -> 'data -> unit) -> ('key, 'data, ('key, 'name) order) t -> unit
(** [iter f m] applies [f] to all bindings in map [m].
   [f] receives the 'key as first argument, and the associated value
   as second argument.  The bindings are passed to [f] in increasing
   order with respect to the ordering over the type of the keys.
   Only current bindings are presented to [f]:
   bindings hidden by more recent bindings are not passed to [f]. *)

val reverse_iter: ('key -> 'data -> unit) -> ('key, 'data, ('key, 'name) order) t -> unit
(** Same as [iter f m] but in decreasing order. *)

val map: ('data1 -> 'data2) -> ('key, 'data1, ('key, 'name) order) t -> ('key, 'data2, ('key, 'name) order) t
(** [map f m] returns a map with same domain as [m], where the
   associated value [a] of all bindings of [m] has been
   replaced by the result of the application of [f] to [a].
   The bindings are passed to [f] in increasing order
   with respect to the ordering over the type of the keys. *)

val mapi: ('key -> 'data1 -> 'data2) -> ('key, 'data1, ('key, 'name) order) t -> ('key, 'data2, ('key, 'name) order) t
(** Same as {!Map.S.map}, but the function receives as arguments both the
   'key and the associated value for each binding of the map. *)

val fold: ('key -> 'data -> 'b -> 'b) -> ('key, 'data, ('key, 'name) order) t -> 'b -> 'b
(** [fold f m a] computes [(f kN dN ... (f k1 d1 a)...)],
   where [k1 ... kN] are the keys of all bindings in [m]
   (in increasing order), and [d1 ... dN] are the associated data. *)

val reverse_fold: ('key -> 'data -> 'b -> 'b) -> ('key, 'data, ('key, 'name) order) t -> 'b -> 'b
(** Same as [fold f m a] but in decreasing order *)

val compare: ('key, 'name) order -> ('data -> 'data -> int) -> ('key, 'data, ('key, 'name) order) t -> ('key, 'data, ('key, 'name) order) t -> int
(** Total ordering between maps.  The first argument is a total ordering
    used to compare data associated with equal keys in the two maps. *)

val equal: ('key, 'name) order -> ('data -> 'data -> bool) -> ('key, 'data, ('key, 'name) order) t -> ('key, 'data, ('key, 'name) order) t -> bool
(** [equal cmp m1 m2] tests whether the maps [m1] and [m2] are
   equal, that is, contain equal keys and associate them with
   equal data.  [cmp] is the equality predicate used to compare
   the data associated with the keys. *)

val min_key : ('key, 'data, ('key, 'name) order) t -> 'key
(** [min_key m] returns the smallest 'key in the map [m], or raises
   [Not_found] if [m] is empty. *)

val max_key : ('key, 'data, ('key, 'name) order) t -> 'key
(** [min_key m] returns the largest 'key in the map [m], or raises
   [Not_found] if [m] is empty. *)

val choose_key : ('key, 'data, ('key, 'name) order) t -> 'key
(** [choose_key m] returns an arbitrary 'key from the map [m] in O(1) time. *)

val choose : ('key, 'name) order -> ('key, 'data, ('key, 'name) order) t -> 'data
(** [choose m] returns an arbitrary element from the map [m] in O(1) time. *)

val length : ('key, 'data, ('key, 'name) order) t -> int
(** [length m] returns the number of entries in [m] *)

val pairs : ('key, 'data, ('key, 'name) order) t -> ('key * 'data) list
(** [pairs m] returns a list of ('key, value) pairs from the map [m], in some
   unspecified order. *)

val t_of_sexp : ('key, 'name) order -> (Sexplib.Sexp.t -> 'key) -> (Sexplib.Sexp.t -> 'data) -> Sexplib.Sexp.t -> ('key, 'data, ('key, 'name) order) t

val sexp_of_t : ('key -> Sexplib.Sexp.t) -> ('data -> Sexplib.Sexp.t) -> ('key, 'data, ('key, 'name) order) t -> Sexplib.Sexp.t

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

module Gen :
  sig
    module Gmap : GMAP
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

module Make : functor(O : ORDERED) -> S with type key = O.t
