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
    match a>0 with
      | true -> let hour = a/3600 in
                        let min = System.Convert.ToInt32(((float(a)/3600.0)-float(hour))*60.0) in 
                            let second = System.Convert.ToInt32(((((float(a)/3600.0)-float(hour))*60.0)-float(min))*60.0) in 
                                (hour,min,second)
      | false -> (0,0,0)
       

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

let month a =
    match (a>=1 && a<=12,a) with
        | (true,1) ->("January", 31)
        | (true,2) -> ("February", 28)
        | (true,3) -> ("March", 31)
        | (true,4) -> ("April", 30)
        | (true,5) -> ("May", 31)
        | (true,6) -> ("June", 30)
        | (true,7) -> ("July", 31)
        | (true,8) -> ("August", 31)
        | (true,9) -> ("September", 30)
        | (true,10) -> ("October", 31)
        | (true,11) -> ("November", 30)
        | (true,12) -> ("December", 31)
        | (false,_) -> failwith "Not a valid month"
                    

let toBinary a =
    match a >= 0 with
       | true ->let rec bin b c = 
                    let b = a%2 
                    let c = c + string(b)
                    let a = a/2
                    match a > 0 with
                     |true -> bin a c
                     |false -> c
                bin a ""      
       | false -> failwith "Cannot use negative numbers"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"