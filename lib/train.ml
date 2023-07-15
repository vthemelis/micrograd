open Core
open Nn

module Gradient_descent =
  struct
    type t =
      { learning_rate : float
      ; loss_generator : unit -> Node.t
      }

    let make learning_rate loss_generator =
      { learning_rate
      ; loss_generator
      }

    let train
      (type a)
      (module M : Module with type t = a)
      (training_params : t)
      (module_ : M.t)
      n_epochs
    =
      let run_epoch (n: Node.t) =
        let open Node in
        let model_params = module_ |> M.parameters in
        model_params
          |> Sequence.iter ~f:(fun x ->
            x.grad <- 0.0
          );
        Node.backward n;
        model_params
          |> Sequence.iter ~f:(fun x ->
            let rate = training_params.learning_rate in
            x.value <- x.value -. (rate *. x.grad)
          )
      in

      for _ = 0 to n_epochs do
        let loss = training_params.loss_generator () in
        run_epoch loss;
      done
  end
