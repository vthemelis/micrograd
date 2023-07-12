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
  | Exp of { operand : t; exponent : int }
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

let (~-) x  = ~> ~-. 1.0 * x

let (-) x y = x + ~- y

let ( ** ) x y =
  { value = Float.int_pow x.value y
  ; grad = 0.0
  ; id = Uid.create ()
  ; op = Exp { operand = x; exponent = y }
  }

let (/) x y = ~> x * (y ** (Int.neg 1))

let backward (x: t) =
  let backward_pass_step (x: t) =
    match x.op with
    | Add (l, r) ->
      l.grad <- l.grad +. x.grad;
      r.grad <- r.grad +. x.grad
    | Mul (l, r) ->
      l.grad <- l.grad +. r.value *. x.grad;
      r.grad <- r.grad +. l.value *. x.grad
    | Exp { operand; exponent } ->
      operand.grad <- Float.(operand.grad +. (of_int exponent *. (int_pow x.value @@ Int.(exponent - 1)) ))
    | Tanh operand ->
      operand.grad <- operand.grad +. x.grad *. (1.0 -. x.value *. x.value)
    | Var -> () (* TODO Var shouldn't have a gradient *)
  in

  let rec topo (x: t) (visited: UidHS.t) accum : t list =
    if Hash_set.mem visited x.id then
      accum
    else begin
      Hash_set.add visited x.id;
      match x.op with
      | Add (l, r) | Mul (l, r) ->
        x :: (topo r visited @@ topo l visited @@ accum)
      | Exp { operand; _ } | Tanh operand ->
        x :: (topo operand visited @@ accum)
      | Var -> x :: accum
    end
  in
  topo x (UidHS.create ()) [] |> List.iter ~f:(fun n -> n |> backward_pass_step)
