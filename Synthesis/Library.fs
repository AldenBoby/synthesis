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

let min a b =
    match a>b with
      |true -> b
      |false -> a

let max a b =
    match a>b with
      |true -> a
      |false -> b

let ofTime a b c =
    (a*60*60)+(b*60)+c

let toTime a =
      let hour = int(a/(60*60)) in
          let min = int(((float(a/(60*60)))-float(hour))*60.0) in 
              let second = int(((((float(a/(60*60)))-float(hour))*60.0)-float(min))*60.0) in 
                   (hour,min,second) 

let digits a =
    let string = a.ToString() in
          match string.[0] with
              | '-' -> string.Length-1 // gets rid of negative sign
              | _ -> string.Length
             

let minmax (a,b,c,d) =
    let low = min (min a b) (min c d) in 
            let high = max (max a b) (max c d) in 
                 (low,high)

let isLeap a =
    match a < 1582 with
        |true -> failwith "Year cannot be less than 1582"
        |false -> match a%4=0 && a%100 <> 0 || a%4 =0 && a%100 = 0 && a%400 = 0  with
                    | true -> true
                    | false -> false

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