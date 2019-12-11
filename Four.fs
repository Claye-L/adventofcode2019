module Four
let inputRange = [178416..676461]

let isSixDigit x= x < 1000000 && x >= 100000
let rec digits x = 
    match x > 10 with
    |true -> Seq.append (digits (x/10))  [(x % 10)]
    |false -> Seq.ofList [x]
let hasDouble x = 
    digits x |> Seq.pairwise |> Seq.map (fun (a,b) -> a=b) |> Seq.contains true
let isIncreasingOrder x = 
    digits x |> Seq.pairwise |> Seq.map (fun (a,b) -> a <= b) |>  Seq.contains false |> not
let isValidPassword x =
    isSixDigit x && hasDouble x && isIncreasingOrder x 

let FourOne =
    inputRange |> Seq.filter isValidPassword |> Seq.length

let hasDoubleNotGrouped x =
    let (_,_,res) = Seq.append (digits x) [-1] 
                    |> Seq.fold (fun (dig,count,valid) d -> if d = dig then dig,count+1,valid else d,1, valid || count = 2) (0,0,false)
    res
    
let FourTwo =
     inputRange |> Seq.filter isValidPassword |> Seq.filter hasDoubleNotGrouped |> Seq.length
