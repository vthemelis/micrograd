open Core

let () =
  let open Node in
  let a = ~> ~-.2.0 in
  let b = ~> 3.0 in
  let d = a * b in
  let e = a + b in
  let f = d * e in
  (*
  let b = ~> ~-.3.0 in
  let c = ~> 10.0 in
  let x = a * b + c in
  *)
  let x = a + a in
  f.grad <- 1.0;
  printf "%s\n" @@ show f;
  f |> backward;
  printf "%s\n" @@ show f;
