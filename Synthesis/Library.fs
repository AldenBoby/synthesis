module Synthesis

let abelar a =
    a>12 && a<3097 && a%12=0

let area a b =
    match a<0.0 || b<0.0 with
      |true -> failwith "cannot enter negative numbers"
      |false -> a*b*(0.5)

let zollo a =
    match a < 0 with
      |true -> -a
      |false -> a*2

let min _ _ =
    failwith "Not implemented"

let max _ _ =
    failwith "Not implemented"

let ofTime _ _ _ =
    failwith "Not implemented"

let toTime _ =
    failwith "Not implemented"

let digits _ =
    failwith "Not implemented"

let minmax _ =
    failwith "Not implemented"

let isLeap _ =
    failwith "Not implemented"

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"