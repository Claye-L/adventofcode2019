module IntCodeComputerTests
open IntCodeComputer

let ass a b message =
    if a = b then () else failwith message

let tEq0 =
    let data = [3;9;8;9;10;9;4;9;99;-1;8]
    ass ( computeProgram data [0] [] 0 |> snd |> List.head) 0
let tEq1 =
    let data = [3;9;8;9;10;9;4;9;99;-1;8]
    ass ( computeProgram data [8] [] 0 |> snd |> List.head) 1
let tLt0 = 
    let data = [3;9;7;9;10;9;4;9;99;-1;8]
    ass ( computeProgram data [7] [] 0 |> snd |> List.head) 1
let tLt1 = 
    let data = [3;9;7;9;10;9;4;9;99;-1;8]
    ass ( computeProgram data [9] [] 0 |> snd |> List.head) 0
let tEq2 =
    let data = [3;3;1108;-1;8;3;4;3;99]
    ass ( computeProgram data [0] [] 0 |> snd |> List.head) 0
let tEq3 =
    let data = [3;3;1108;-1;8;3;4;3;99]
    ass ( computeProgram data [8] [] 0 |> snd |> List.head) 1
let tLt2 = 
    let data = [3;3;1107;-1;8;3;4;3;99]
    ass ( computeProgram data [7] [] 0 |> snd |> List.head) 1
let tLt3 = 
    let data = [3;3;1107;-1;8;3;4;3;99]
    ass ( computeProgram data [9] [] 0 |> snd |> List.head) 0

let tests =
    tEq0 "fail teq0"
    tEq1 "fail teq1"
    tEq2 "fail teq2"
    tEq3 "fail teq3"
    tLt0 "fail tlt0"
    tLt1 "fail tlt1"
    tLt2 "fail tlt2"
    tLt3 "fail tlt3"
