open Core
open Micrograd
open Nn

let run_epoch ?(rate=0.1) (n: Node.t) (params: Node.t Sequence.t) =
  let open Node in
  params
    |> Sequence.iter ~f:(fun x ->
      x.grad <- 0.0
    );
  Node.backward n;
  params
    |> Sequence.iter ~f:(fun x ->
      x.value <- x.value -. (rate *. x.grad)
    )

let () =
  (* Random.init 3778; *)
  let open Node in
  let xs = [|
    [|2.0; 3.0; ~-.1.0|];
    [|3.0; ~-.1.0; 0.5|];
    [|0.5; 1.0; 1.0|];
    [|1.0; 1.0; -1.0|]
  |] in
  let y = [|1.0; ~-.1.0; ~-.1.0; 1.0|] in
  let mlp = MLP.make 3 [4; 6; 4; 1] in

  printf "MLP has %d params\n" @@ Sequence.length @@ MLP.parameters mlp;

  let xs_vals = xs
    |> Array.map ~f:(fun x -> x |> Array.map ~f:(~>)) in

  for i = 1 to 10000 do
    let y_out = xs_vals
      |> Array.map ~f:(fun x -> Array.get MLP.(mlp.%(x)) 0) in
    let loss = y
      |> Array.zip_exn y_out
      |> Array.map ~f:(fun (yout, y) -> let y = ~>y in (yout - y) ** 2)
      |> Array.reduce_exn ~f:(+) in
    let open MLP in
    run_epoch loss @@ parameters mlp;
    if i % 100 = 0 then begin
      printf "New loss: %f\n" loss.value
    end;
    ()
  done;

  let y_out = xs_vals
    |> Array.map ~f:(fun x -> Array.get MLP.(mlp.%(x)) 0) in

  y
    |> Array.to_list |> List.to_string ~f:Float.to_string
    |> print_endline;

  y_out
    |> Array.map ~f:(fun v -> v.value)
    |> Array.to_list |> List.to_string ~f:Float.to_string
    |> print_string;

