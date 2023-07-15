open Core
open Node

module type Module =
  sig
    type t
    val parameters : t -> Node.t Sequence.t
  end

module Neuron =
  struct
    type t =
      { weights : Node.t array
      ; bias : Node.t
      }

    (* Make a neuron of n inputs *)
    let make n =
      { weights = n |> Array.init ~f:(fun _ -> ~> (Random.float_range (~-. 1.0) 1.0))
      ; bias = ~> (Random.float_range (~-.1.0) 1.0)
      }

    (* Apply the neuron on a vector x
     * Equivalent to (w * x + b) *)
    let (.%()) neuron (x: Node.t array) =
      tanh ((neuron.weights
        |> Array.zip_exn x
        |> Array.map ~f:(fun (x, w) -> w * x)
        |> Array.reduce_exn ~f:(+))
      + neuron.bias)

    let parameters neuron =
       Sequence.append
        (neuron.bias |> Sequence.return)
        (neuron.weights |> Array.to_sequence)
  end

module Layer =
  struct
    type t =
      { neurons : Neuron.t array
      }

    (* Make a layer of nout neurons of nin inputs each *)
    let make nin nout =
      { neurons = nout |> Array.init ~f:(fun _ -> Neuron.make nin)
      }

    (* Apply a vector x on each of the neuron of the layer *)
    let (.%()) layer (x: Node.t array) =
      layer.neurons
        |> Array.map ~f:Neuron.(fun neuron -> neuron.%(x))

    let parameters layer =
      layer.neurons
        |> Array.to_sequence
        |> Sequence.map ~f:Neuron.parameters
        |> Sequence.concat
  end

module MLP =
  struct
    type t =
      { layers : Layer.t list
      }

    let make nin nout =
      let layers = nin :: nout
        |> fun xs -> List.take xs @@ List.length nout
        |> List.zip_exn nout
        |> List.map ~f:(fun (j, i) -> Layer.make i j) in
      { layers }

    let (.%()) mlp (x: Node.t array) =
      mlp.layers |> List.fold ~init:x ~f:(fun x l -> Layer.(l.%(x)))

    let parameters mlp =
      mlp.layers
        |> Sequence.of_list
        |> Sequence.map ~f:Layer.parameters
        |> Sequence.concat
  end
