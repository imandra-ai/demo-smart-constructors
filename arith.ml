
type var = string

type aexpr =
  | Var of string
  | Const of int
  | Plus of aexpr * aexpr
  | Mult of aexpr * aexpr
  | Neg of aexpr
  | Pow of aexpr * int
  | Sum of { x: var; low: aexpr; up: aexpr; body: aexpr } (* x bound in body *)

and bexpr =
  | Bool of bool
  | Not of bexpr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Eq of aexpr * aexpr

module Var_set = Set.Make(String)
module Var_map = Map.Make(String)

let simpl = ref true

type subst = aexpr Var_map.t

module Aexpr = struct
  type t = aexpr

  let var v = Var v
  let const n = Const n

  let rec plus a b =
    match a, b with
    | _ when not !simpl -> Plus (a,b)
    | Const 0, _ -> b
    | _, Const 0 -> a
    | Const x, Const y -> Const (x+y)
    | _, Const _ -> plus b a (* put constant first *)
    | Const x, Plus (Const y, e) -> plus (const (x+y)) e
    | _ -> Plus (a,b)

  let rec mult a b =
    match a, b with
    | _ when not !simpl -> Mult (a,b)
    | Const 0, _
    | _, Const 0 -> const 0
    | Const 1, _ -> b
    | _, Const 1 -> a
    | Const x, Const y -> Const (x*y)
    | _, Const _ -> mult b a (* put constant first *)
    | Const x, Mult (Const y, e) -> mult (const (x*y)) e
    | _ -> Mult (a,b)

  let rec neg = function
    | x when not !simpl -> Neg x
    | Const n -> Const (-n)
    | Neg x -> x
    | Plus (x,y) -> plus (neg x) (neg y)
    | Mult (x,y) -> mult (neg x) y
    | x -> Neg x

  let minus x y = plus x (neg y)

  (* compute integer power *)
  let pow_ a b =
    let rec aux acc = function
      | 1 -> acc
      | n ->
        if n mod 2 = 0
        then aux (acc*acc) (n/2)
        else acc * (aux (acc*acc) (n/2))
    in
    match b with
    | 0 -> if a = 0 then raise (Invalid_argument "pow: undefined value 0^0") else 1
    | b when b < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
    | b -> aux a b

  let pow x y = match x with
    | Const n -> const (pow_ n y)
    | _ when y=1 -> x
    | Pow (x', y') -> Pow (x', y' * y)
    | _ -> Pow (x,y)

  (* [sum] needs [eval] so we make them mutually recursive *)
  let rec sum x ~low ~up body : t =
    match low, up with
    | Const i, Const j ->
      let r = ref (const 0) in
      for k = i to j do
        r := plus !r (eval (Var_map.singleton x (const k)) body)
      done;
      !r
    | _ -> Sum {x; low; up; body}

  and map ~f e : t =
    match e with
    | Const _ -> e
    | Var v -> f v
    | Plus (x,y) -> plus (map ~f x) (map ~f y)
    | Mult (x,y) -> mult (map ~f x) (map ~f y)
    | Neg x -> neg (map ~f x)
    | Pow (x,y) -> pow (map ~f x) y
    | Sum {x;low;up;body} ->
      sum x ~low:(map ~f low) ~up:(map ~f up) (map ~f body)

  and eval subst e =
    map e
      ~f:(fun v -> match Var_map.find v subst with
         | t -> eval subst t
         | exception Not_found -> var v)

  let (~-) = neg
  let (^) = pow
  let (+) = plus
  let (-) = minus
  let ( * ) = mult (* the "( * )" is necessary for this not to be a comment *)

  let rec pp out (e:aexpr) : unit =
    match e with
    | Const n -> Format.fprintf out "%d" n
    | Var v -> Format.fprintf out "%s" v
    | Plus (x, Neg y) -> Format.fprintf out "(@[%a@ - %a@])" pp x pp y
    | Plus (x,y) -> Format.fprintf out "(@[%a@ + %a@])" pp x pp y
    | Mult (x,y) -> Format.fprintf out "(@[%a@ * %a@])" pp x pp y
    | Neg x -> Format.fprintf out "@[~- %a@]" pp x
    | Pow (x,y) -> Format.fprintf out "(@[%a@ ^ %d@])" pp x y
    | Sum {x;low;up;body} ->
      Format.fprintf out "(@[@<1>Σ_{@[%s=%a@]}^{@[%a@]}@ %a@])" x pp low pp up pp body

  (* NOTE: this would be more efficient with a "fold"-like iterator
     that would carry a growing set around *)
  let rec vars = function
    | Const _ -> Var_set.empty
    | Var v -> Var_set.singleton v
    | Plus (x,y) | Mult (x,y) -> Var_set.union (vars x) (vars y)
    | Neg x | Pow(x,_) -> vars x
    | Sum {x;low;up;body} ->
      Var_set.union
        (Var_set.union (vars low) (vars up))
        (Var_set.remove x @@ vars body) (* x is bound in body! *)
end

module Bexpr = struct
  type t = bexpr

  let bool b = Bool b
  let true_ = Bool true
  let false_ = Bool false
  let not_ = function
    | x when not !simpl -> Not x
    | Bool b -> bool (not b)
    | Not x -> x
    | x -> Not x

  let and_ x y =
    match x, y with
    | _ when not !simpl -> And (x,y)
    | Bool false, _
    | _, Bool false -> false_
    | Bool true, Bool true -> true_
    | _ -> And (x,y)

  let or_ x y =
    match x, y with
    | _ when not !simpl -> Or (x,y)
    | Bool true, _
    | _, Bool true -> true_
    | Bool false, Bool false -> false_
    | _ -> Or (x,y)

  let eq x y =
    match x, y with
    | _ when not !simpl -> Eq (x,y)
    | Const a, Const b -> bool (a=b)
    | _ -> Eq (x,y)

  let neq x y = not_ (eq x y)

  let (&&) = and_
  let (||) = or_
  let (=) = eq
  let (<>) = neq

  let rec pp out e : unit =
    match e with
    | Bool b -> Format.fprintf out "%B" b
    | Not x -> Format.fprintf out "@[not@ %a@])" pp x
    | And (a,b) -> Format.fprintf out "(@[%a@ && %a@])" pp a pp b
    | Or (a,b) -> Format.fprintf out "(@[%a@ || %a@])" pp a pp b
    | Eq (a,b) -> Format.fprintf out "(@[%a@ = %a@])" Aexpr.pp a Aexpr.pp b

  let rec map ~f e : t =
    match e with
    | Bool _ -> e
    | And (x,y) -> and_ (map ~f x) (map ~f y)
    | Or  (x,y) -> or_ (map ~f x) (map ~f y)
    | Not x -> not_ (map ~f x)
    | Eq (x,y) -> eq (f x) (f y)

  let eval subst e =
    map e ~f:(fun v -> Aexpr.eval subst v)

  let rec vars = function
    | Bool _ -> Var_set.empty
    | And (x,y) | Or (x,y) -> Var_set.union (vars x)(vars y)
    | Not x -> vars x
    | Eq (x,y) -> Var_set.union (Aexpr.vars x)(Aexpr.vars y)
end

module Subst = struct
  module M = Var_map
  type t = subst
  let empty = M.empty
  let singleton = M.singleton

  let pp out m =
    Format.fprintf out "{@[";
    let first = ref true in
    M.iter
      (fun v t ->
         if !first then first := false else Format.fprintf out ",@ ";
         Format.fprintf out "@[%s -> %a@]" v Aexpr.pp t)
      m;
    Format.fprintf out "@]}"

  let add v t s =
    if M.mem v s then failwith (Format.asprintf "var %s already bound in %a" v pp s);
    M.add v t s

  let of_list l = List.fold_right (fun (v,t) s -> add v t s) l empty
  let to_list = M.bindings
end

let rec derive x e =
  let open Aexpr in
  match e with
  | Const _ -> const 0
  | Var v when x=v -> const 1
  | Var _ -> const 0
  | Neg a -> ~- (derive x a)
  | Pow (a,p) ->
    const p * (pow a Pervasives.(p-1)) * derive x a
  | Plus (a,b) ->
    derive x a + derive x b
  | Mult (a,b) ->
    derive x a * b + a * derive x b
  | Sum r when r.x = x -> const 0 (* cannot contain "x" free, so it's a constant *)
  | Sum {x=y;low;up;body} ->
    sum y ~low:(derive x low) ~up:(derive x up) (derive x body)

let e1 = Aexpr.(var "x" * const 2 + const 3 * (var "y" ^ 10) )

let e2 = Aexpr.(const 3 * e1 + e1 * const 2 + var "z" ^ 2)

let e3 = Aexpr.(e1 * var "x" - e2 +
                var "x" * sum "x" ~low:(const 1) ~up:(var "x") e1)

let b1 = Bexpr.(e2 = e2 || e1 <> e3 ||
                not_ (not_ (Aexpr.var "x" = Aexpr.var "y" && Aexpr.var "x" = Aexpr.var "z")))

let s1 = Subst.of_list ["x", Aexpr.const 3; "y", Aexpr.(var "x" + const 2)]

let s2 = Subst.of_list ["x", Aexpr.(const 2 * var "y"); "y", Aexpr.(var "z" + const 2); "z", Aexpr.const 10]
