module TwoOne

open System

let rawData = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,10,31,35,2,6,35,39,1,39,13,43,1,43,9,47,2,47,10,51,1,5,51,55,1,55,10,59,2,59,6,63,2,6,63,67,1,5,67,71,2,9,71,75,1,75,6,79,1,6,79,83,2,83,9,87,2,87,13,91,1,10,91,95,1,95,13,99,2,13,99,103,1,103,10,107,2,107,10,111,1,111,9,115,1,115,2,119,1,9,119,0,99,2,0,14,0"
let data = rawData.Split(',') |> Seq.map int |> Seq.toList

let testOpCode = [1;9;10;3;2;3;11;0;99;30;40;50]
let test0 = ([1;0;0;0;99],[2;0;0;0;99])
let test1 = [2;3;0;3;99],[2;3;0;6;99]
let test2 = [2;4;4;5;99;0],[2;4;4;5;99;9801]
let test3 = [1;1;1;4;99;5;6;0;99],[30;1;1;4;2;5;6;0;99]

type Instruction = |Add |Mult |Finish

let addOperation (input: int list) index = 
    let value = input.[input.[index + 1]] + input.[input.[index + 2]]
    let pos = input.[index + 3]
    input |> List.mapi (fun i item -> if i = pos then value else item)
let multOperation (input: int list) index =
    let value = input.[input.[index + 1]] * input.[input.[index + 2]]
    let pos = input.[index + 3]
    input |> List.mapi (fun i item -> if i = pos then value else item)

let readOpCode (input: int list) index=
    match input.[index] with
    |1 -> Add
    |2 -> Mult
    |99 -> Finish
    |_ -> failwith "lmao"
    
    
let rec computeProgram (input: int list) index=
    match readOpCode input index with
    |Finish -> input
    |Add -> computeProgram (addOperation input index) (index + 4)
    |Mult -> computeProgram (multOperation input index) (index + 4)
let validate input result = (computeProgram input 0 ) |> Seq.compareWith (fun a b -> if a = b then 0 else 1) result  |> (=) 0
let tests = 
    [test0;test1;test2;test3] |> Seq.map (fun (input,result) -> validate input result) |> Seq.toList

let replaceAtIndex index value =
    List.mapi (fun i item -> if i = index then value else item)
let changeParams val1 val2 =
    replaceAtIndex 1 val1  >> replaceAtIndex 2 val2

let guess val1 val2 = data |> changeParams val1 val2 
let guesses = 
    [
        for i in [0..128] do
            for j in [0..128] do
                yield i,j
    ]
    |> List.map (fun (a,b) -> (a,b), guess a b)

let TwoOne =
    let res g = computeProgram g 0
    let strings = guesses |> List.map (fun (input,g) -> input,(res g).Head)
                |> List.filter (fun (_,head) -> head = 19690720)
                |> List.map (fun ((a,b),_) -> sprintf "%d %d" a b)
    String.Join("\n",strings)    

