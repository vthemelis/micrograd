open Core
open Micrograd
open Node

let%expect_test "addition" =
  print_string @@ Node.show @@ (~> ~-. 1.0 + ~>2.0);
  [%expect {|
    { Node.value = 1.; grad = 0.; id = <opaque>;
      op =
      (Node.Add ({ Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var },
         { Node.value = 2.; grad = 0.; id = <opaque>; op = Node.Var }))
      } |}]

let%expect_test "subtraction" =
  print_string @@ Node.show @@ (~> ~-. 1.0 - ~>2.0);
  [%expect {|
    { Node.value = -3.; grad = 0.; id = <opaque>;
      op =
      (Node.Add ({ Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var },
         { Node.value = -2.; grad = 0.; id = <opaque>;
           op =
           (Node.Mul (
              { Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var },
              { Node.value = 2.; grad = 0.; id = <opaque>; op = Node.Var }))
           }
         ))
      } |}]

let%expect_test "multiplication" =
  print_string @@ Node.show @@ (~> ~-. 1.0 * ~>2.0);
  [%expect {|
    { Node.value = -2.; grad = 0.; id = <opaque>;
      op =
      (Node.Mul ({ Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var },
         { Node.value = 2.; grad = 0.; id = <opaque>; op = Node.Var }))
      } |}]

let%expect_test "negation" =
  print_string @@ Node.show @@ (~- ~> ~-. 1.0);
  [%expect {|
    { Node.value = 1.; grad = 0.; id = <opaque>;
      op =
      (Node.Mul ({ Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var },
         { Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var }))
      } |}]

let%expect_test "negation" =
  print_string @@ Node.show @@ (~- ~> ~-. 1.0);
  [%expect {|
    { Node.value = 1.; grad = 0.; id = <opaque>;
      op =
      (Node.Mul ({ Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var },
         { Node.value = -1.; grad = 0.; id = <opaque>; op = Node.Var }))
      } |}]

let%expect_test "exponentiation" =
  print_string @@ Node.show @@ (~>2.0 ** 10);
  [%expect {|
    { Node.value = 1024.; grad = 0.; id = <opaque>;
      op =
      Node.Poly {
        operand = { Node.value = 2.; grad = 0.; id = <opaque>; op = Node.Var };
        exponent = 10}
      } |}]

let%expect_test "tanh" =
  print_string @@ Node.show @@ (tanh ~> ~-. 2.0);
  [%expect {|
    { Node.value = -0.964027580076; grad = 0.; id = <opaque>;
      op =
      (Node.Tanh { Node.value = -2.; grad = 0.; id = <opaque>; op = Node.Var }) } |}]

let%expect_test "backprogation" =
  let a = ~>3.0 in
  let b = ~> 2.0 in
  let c = a + b in
  let d = a * b in
  let e' = c + d in
  let e = e' ** 2 in
  print_string @@ Node.show e;
  [%expect {|
    { Node.value = 121.; grad = 0.; id = <opaque>;
      op =
      Node.Poly {
        operand =
        { Node.value = 11.; grad = 0.; id = <opaque>;
          op =
          (Node.Add (
             { Node.value = 5.; grad = 0.; id = <opaque>;
               op =
               (Node.Add (
                  { Node.value = 3.; grad = 0.; id = <opaque>; op = Node.Var },
                  { Node.value = 2.; grad = 0.; id = <opaque>; op = Node.Var }))
               },
             { Node.value = 6.; grad = 0.; id = <opaque>;
               op =
               (Node.Mul (
                  { Node.value = 3.; grad = 0.; id = <opaque>; op = Node.Var },
                  { Node.value = 2.; grad = 0.; id = <opaque>; op = Node.Var }))
               }
             ))
          };
        exponent = 2}
      } |}];

  print_string
    @@ List.to_string ~f:(fun x -> Float.to_string x.value)
    @@ Node.topo e;
  [%expect {| (121. 11. 6. 5. 2. 3.) |}];

  Node.backward e;

  print_string @@ Node.show e;
  [%expect {|
    { Node.value = 121.; grad = 1.; id = <opaque>;
      op =
      Node.Poly {
        operand =
        { Node.value = 11.; grad = 22.; id = <opaque>;
          op =
          (Node.Add (
             { Node.value = 5.; grad = 22.; id = <opaque>;
               op =
               (Node.Add (
                  { Node.value = 3.; grad = 66.; id = <opaque>; op = Node.Var },
                  { Node.value = 2.; grad = 88.; id = <opaque>; op = Node.Var }))
               },
             { Node.value = 6.; grad = 22.; id = <opaque>;
               op =
               (Node.Mul (
                  { Node.value = 3.; grad = 66.; id = <opaque>; op = Node.Var },
                  { Node.value = 2.; grad = 88.; id = <opaque>; op = Node.Var }))
               }
             ))
          };
        exponent = 2}
      } |}]

let%expect_test "backprogation 2" =
  let a = ~> ~-.2.0 in
  let b = ~> 3.0 in
  let d = a * b in
  let e = a + b in
  let f = d * e in
  f |> Node.backward;

  print_endline @@ Node.show f;
  [%expect {|
    { Node.value = -6.; grad = 1.; id = <opaque>;
      op =
      (Node.Mul (
         { Node.value = -6.; grad = 1.; id = <opaque>;
           op =
           (Node.Mul (
              { Node.value = -2.; grad = -3.; id = <opaque>; op = Node.Var },
              { Node.value = 3.; grad = -8.; id = <opaque>; op = Node.Var }))
           },
         { Node.value = 1.; grad = -6.; id = <opaque>;
           op =
           (Node.Add (
              { Node.value = -2.; grad = -3.; id = <opaque>; op = Node.Var },
              { Node.value = 3.; grad = -8.; id = <opaque>; op = Node.Var }))
           }
         ))
      } |}]

let%expect_test "backprogation 3" =
  let x1 = ~> 2.0 in
  let x2 = ~> 0.0 in
  let w1 = ~> ~-. 3.0 in
  let w2 = ~> 1.0 in
  let b = ~> 6.8813735870195432 in
  let x1w1 = x1 * w1 in
  let x2w2 = x2 * w2 in
  let s = x1w1 + x2w2 in
  let n = s + b in
  let o = tanh n in

  o |> Node.backward;
  print_string @@ Node.show o;
  [%expect {|
    { Node.value = 0.707106781187; grad = 1.; id = <opaque>;
      op =
      (Node.Tanh
         { Node.value = 0.88137358702; grad = 0.5; id = <opaque>;
           op =
           (Node.Add (
              { Node.value = -6.; grad = 0.5; id = <opaque>;
                op =
                (Node.Add (
                   { Node.value = -6.; grad = 0.5; id = <opaque>;
                     op =
                     (Node.Mul (
                        { Node.value = 2.; grad = -1.5; id = <opaque>;
                          op = Node.Var },
                        { Node.value = -3.; grad = 1.; id = <opaque>;
                          op = Node.Var }
                        ))
                     },
                   { Node.value = 0.; grad = 0.5; id = <opaque>;
                     op =
                     (Node.Mul (
                        { Node.value = 0.; grad = 0.5; id = <opaque>;
                          op = Node.Var },
                        { Node.value = 1.; grad = 0.; id = <opaque>;
                          op = Node.Var }
                        ))
                     }
                   ))
                },
              { Node.value = 6.88137358702; grad = 0.5; id = <opaque>;
                op = Node.Var }
              ))
           })
      } |}]
  