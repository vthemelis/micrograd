open Core

module Uid = Unique_id.Int ()
module UidHS = Hash_set.Make(Uid)

type t =
  { mutable value : float
  ; mutable grad : float
  ; id: Uid.t [@opaque] (* TODO: This is an int, should be able to show it *)
  ; op : op
  }
[@@deriving show]

and op =
  | Add of t * t
  | Mul of t * t
  | Poly of { operand : t; exponent : int }
  | Tanh of t
  | Var
[@@deriving show]

let return x = { value = x; id = Uid.create () ; grad = 0.0; op = Var }

let (~>) x = return x

let (+) x y =
  { value = x.value +. y.value
  ; grad = 0.0
  ; id = Uid.create ()
  ; op = Add (x, y)
  }

let ( * ) x y =
  { value = x.value *. y.value
  ; grad = 0.0
  ; id = Uid.create ()
  ; op = Mul (x, y)
  }

let (~-) x  = (~> (~-. 1.0)) * x

let (-) x y = x + (~- y)

let ( ** ) x y =
  { value = Float.int_pow x.value y
  ; grad = 0.0
  ; id = Uid.create ()
  ; op = Poly { operand = x; exponent = y }
  }

let (/) x y = (~> x) * (y ** (Int.minus_one) )

let tanh x =
  { value = Float.tanh x.value
  ; grad = 0.0
  ; id = Uid.create ()
  ; op = Tanh x
  }

let topo x =
  let rec topo (x: t) (visited: UidHS.t) accum : t list =
    if Hash_set.mem visited x.id then
      accum
    else begin
      Hash_set.add visited x.id;
      match x.op with
      | Add (l, r) | Mul (l, r) ->
        x :: (topo r visited @@ topo l visited @@ accum)
      | Poly { operand; _ } | Tanh operand ->
        x :: (topo operand visited @@ accum)
      | Var -> x::accum
    end
  in
  topo x (UidHS.create ()) []

let backward_impl nodes =
  let backward_pass_step (x: t) =
    match x.op with
    | Add (l, r) ->
      l.grad <- l.grad +. x.grad;
      r.grad <- r.grad +. x.grad
    | Mul (l, r) ->
      l.grad <- l.grad +. r.value *. x.grad;
      r.grad <- r.grad +. l.value *. x.grad
    | Poly { operand; exponent } ->
      let open Float in
      let exp = of_int exponent in
      let exp_minus_one = Int.(exponent - 1) in
      operand.grad <- operand.grad +. x.grad *. exp *. (int_pow operand.value exp_minus_one)
    | Tanh operand ->
      operand.grad <- operand.grad +. x.grad *. (1.0 -. x.value *. x.value)
    | Var -> ()
  in
  nodes |> List.iter ~f:(backward_pass_step)

let backward n =
  n.grad <- 1.0;
  backward_impl @@ topo n
