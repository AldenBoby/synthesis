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
                        let min = a%3600/60 in 
                            let second = (a%3600)%60 in 
                                (hour,min,second)
      | false -> (0,0,0)
       

let digits a =
    (*let string = a.ToString() in
          match string.[0] with
              | '-' -> string.Length-1 // gets rid of negative sign
              | _ -> string.Length*)

     //Apadted from F# Digit Count by ShaunLuttin
     match a = 0 with |true -> 1 |false -> match a <0 with
                                           |true -> let a = -a
                                                    int (log10 ((float)a)) + 1
                                           |false -> int (log10 ((float)a)) + 1
             

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
    match (a>=1 && a<=12,a) with | (false,_) -> failwith "Not a valid month"
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

let toBinary a =
    match a >= 0 with
       | true ->let rec bin b = 
                    match b with
                    | 0|1 -> string(b)
                    | _ -> let output = string(b%2)
                           bin (b/2) + output
                bin a
       | false -> failwith "Cannot use negative numbers"

let bizFuzz n =
     let rec multiplier div3 div5 div35 count=
        match count<=n with
           | false -> (div3,div5,div35)
           | true -> match count%3 = 0 && count%5 = 0 with
                        |true -> multiplier (div3 + 1) (div5 + 1) (div35 + 1) (count + 1)
                        |false -> match count%3 = 0 with
                                     |true -> multiplier (div3 + 1) div5 div35 (count + 1)
                                     |false -> match count%5 = 0 with
                                                  |true -> multiplier div3 (div5 + 1) div35 (count + 1)
                                                  |false ->multiplier div3 div5 div35 (count + 1)
     multiplier 0 0 0 1
                      
                      
                      
           
let monthDay d y =
    let getmonth (a,_) = a
    let getday (_,b) = b
    let rec findmon day start fin mon count leap =
            match (day>=start+leap) && (day<=fin+leap) with
                    |true -> mon
                    |false ->let start=fin
                             match isLeap y with 
                             |true -> findmon day start (fin+(getday (month (count+1)))) (getmonth (month (count+1))) (count+1) 1
                             |false -> findmon day start (fin+(getday (month (count+1)))) (getmonth (month (count+1))) (count+1) 0
                             
    match d>0 && d<=366 && y >=1582 with 
        | false -> failwith "out of bounds"
        | true -> findmon d 1 (getday (month 1)) (getmonth (month 1)) 1 0

let coord (x,y:float) =
    //sqrt function from textbook

    let sqrt n =
        let rec calculate guess i = 
            match i with
            | 10 -> guess
            | _ -> 
                let g = (guess + n/guess) / 2.0
                calculate g (i + 1)
        match n <= 0.0 with 
        | true -> failwith "cannot be less than zero"
        | _ -> calculate (n/2.0) 0

    
    let distance (x1,y1) = sqrt((x1-x)**2.0 + (y1-y)**2.0)

    let square (x1,y1) (width:float) (height:float) = match x >= (x1) && x<=(x1+width)  with |false ->false |true ->true
        
    (distance,square)