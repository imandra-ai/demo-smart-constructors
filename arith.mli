
type var = string

type aexpr = private
  | Var of string
  | Const of int
  | Plus of aexpr * aexpr
  | Mult of aexpr * aexpr
  | Neg of aexpr
  | Pow of aexpr * int
  | Sum of { x: var; low: aexpr; up: aexpr; body: aexpr } (* x bound in body *)

type bexpr = private
  | Bool of bool
  | Not of bexpr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Eq of aexpr * aexpr

module Var_set : module type of Set.Make(String)
module Var_map : module type of Map.Make(String)

(** This boolean is used to enable/disable smart constructors
    [simpl := false] will make all constructors dumb. *)
val simpl : bool ref

(* variable substitution *)
type subst = private aexpr Var_map.t

module Aexpr : sig
  type t = aexpr
  val var : var -> t
  val const : int -> t
  val plus : t -> t -> t
  val mult : t -> t -> t
  val neg : t -> t
  val pow : t -> int -> t
  val minus : t -> t -> t
  val sum : var -> low:t -> up:t -> t -> t

  val ( ~- ) : t -> t (* neg *)
  val ( + ) : t -> t -> t (* add *)
  val ( * ) : t -> t -> t (* mult *)
  val ( ^ ) : t -> int -> t (* pow *)
  val ( - ) : t -> t -> t (* minus *)

  val pp : Format.formatter -> t -> unit

  (** reduce in given valuation *)
  val eval : subst -> t -> t

  val vars : t -> Var_set.t
  val map : f:(var -> t) -> t -> t
end

module Bexpr : sig
  type t = bexpr
  val true_ : t
  val false_ : t
  val bool : bool -> t
  val not_ : t -> t
  val and_ : t -> t -> t
  val or_ : t -> t -> t
  val eq : aexpr -> aexpr -> t
  val neq : aexpr -> aexpr -> t

  val (||) : t -> t -> t
  val (&&) : t -> t -> t
  val (=) : aexpr -> aexpr -> t
  val (<>) : aexpr -> aexpr -> t

  val pp : Format.formatter -> t -> unit

  val map : f:(aexpr -> aexpr) -> t -> t

  (** reduce in given valuation *)
  val eval : subst -> t -> t

  val vars : t -> Var_set.t
end

module Subst : sig
  type t = subst
  val empty : t
  val singleton : var -> aexpr -> t
  val add : var -> aexpr -> t -> t
  val of_list : (var * aexpr) list -> t
  val to_list : t -> (var * aexpr) list
  val pp : Format.formatter -> t -> unit
end

(** Symbolic derivation *)
val derive : var -> aexpr -> aexpr


val e1 : aexpr
val e2 : aexpr
val e3 : aexpr

val b1 : bexpr

val s1 : subst
val s2 : subst
