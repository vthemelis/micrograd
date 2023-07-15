open Core
open Micrograd
open Nn

let () =
  Random.init 3778;

  (* Example MLP training on a small data set *)

  let open Node in

  (* Some input data *)
  let xs = [|
    [|2.0; 3.0; ~-.1.0|];
    [|3.0; ~-.1.0; 0.5|];
    [|0.5; 1.0; 1.0|];
    [|1.0; 1.0; -1.0|]
  |] in

  (* . . . and their corresponding intended predicted value *)
  let y = [|1.0; ~-.1.0; ~-.1.0; 1.0|] in

  (* Initialise a small multi-layer perceptron *)
  let mlp = MLP.make 3 [4; 4; 1] in

  printf "MLP has %d params\n" @@ Sequence.length @@ MLP.parameters mlp;

  let xs_vals = xs
    |> Array.map ~f:(fun x -> x |> Array.map ~f:(~>)) in

  (* Simple square difference loss function *)
  (* loss = sum[ (y_actual - y_predicted) ** 2 ]*)
  let loss_generator = fun () ->
    let y_out = xs_vals
      |> Array.map ~f:(fun x -> Array.get MLP.(mlp.%(x)) 0) in

    y
      |> Array.zip_exn y_out
      |> Array.map ~f:(fun (yout, y) -> let y = ~>y in (yout - y) ** 2)
      |> Array.reduce_exn ~f:(+) in

  let trainer = Train.Gradient_descent.make 0.01 loss_generator in
  Train.Gradient_descent.train (module MLP) trainer mlp 1_000;

  let y_out = xs_vals
    |> Array.map ~f:(fun x -> Array.get MLP.(mlp.%(x)) 0) in

  let learnt_values = y_out
    |> Array.map ~f:(fun v -> v.value)
    |> Array.to_list |> List.to_string ~f:Float.to_string in

  printf "Learnt values: %s" learnt_values;

