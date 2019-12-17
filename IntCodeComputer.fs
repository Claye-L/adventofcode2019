module IntCodeComputer
open AgentProcessings
open AgentProcessings.AgentProcessing

type Argument = |Position of int | Immediate of int |Relative of int
type Instruction = 
    |Add of Argument * Argument * Argument
    |Mult of Argument * Argument * Argument
    |Finish 
    |Input of Argument
    |Output of Argument
    |JumpTrue of Argument * Argument
    |JumpFalse of Argument * Argument
    |LessThan of Argument * Argument * Argument 
    |Equals of Argument * Argument * Argument
    |IncrementBase of Argument

let rec digits x = 
    match x >= 10 with
    |true -> Seq.append (digits (x/10))  [(x % 10)]
    |false -> Seq.ofList [x]

let numToArg x=
    match x with
    |0 -> Position
    |1 -> Immediate
    |2 -> Relative
let read3Args (intcode:int list) index (a,b,c)  =
    (numToArg a) intcode.[index + 1],
    (numToArg b)  intcode.[index + 2],
    (numToArg c)  intcode.[index + 3]
let read2Args (intcode:int list) index (a,b) =
    (numToArg a) intcode.[index + 1],
    (numToArg b)  intcode.[index + 2]
let readArg (intcode: int list) index a =
    (numToArg a)intcode.[index + 1]
    
let readOpCode (intcode: int list) index=
    let opCode = digits intcode.[index] |> Seq.toList
    let twoDigInst = if opCode.Length >= 2 then opCode.[opCode.Length - 2],opCode.[opCode.Length - 1] else 0,List.last opCode
    let padOpCode length = List.append (List.map (fun _->0) [1.. length - opCode.Length]) opCode
    let instr = 
        match twoDigInst with 
        |0,1 -> let padded = padOpCode 5
                Add (read3Args intcode index (padded.[2],padded.[1],padded.[0]))
        |0,2 -> let padded = padOpCode 5 
                Mult (read3Args intcode index (padded.[2],padded.[1],padded.[0]))
        |9,9 -> Finish
        |0,3 -> let padded = padOpCode 3
                Input (readArg intcode index padded.[0])
        |0,4 -> let padded = padOpCode 3
                Output (readArg intcode index padded.[0])
        |0,5 -> let padded = padOpCode 4
                JumpTrue (read2Args intcode index (padded.[1],padded.[0]) )
        |0,6 -> let padded = padOpCode 4
                JumpFalse (read2Args intcode index (padded.[1],padded.[0]) )
        |0,7 -> let padded = padOpCode 5 
                LessThan (read3Args intcode index (padded.[2],padded.[1],padded.[0]))
        |0,8 -> let padded = padOpCode 5 
                Equals (read3Args intcode index (padded.[2],padded.[1],padded.[0]))
        |_,_ -> failwith "Invalid instruction in OpCode"
    instr

let executeAdd (intcode: int list) (a,b,c) =
    let getValue arg = match arg with |Position x -> intcode.[x] |Immediate x -> x
    let value = getValue b + getValue a
    let getPos = match c with |Position x -> x |Immediate x -> x
    intcode |> List.mapi (fun i item -> if i = getPos then value else item)
let executeMult (intcode: int list) (a,b,c) =
    let getValue arg = match arg with |Position x -> intcode.[x] |Immediate x -> x
    let getPos = match c with |Position x -> x |Immediate x -> x
    let value = getValue b * getValue a
    intcode |> List.mapi (fun i item -> if i = getPos  then value else item)
let executeInput (intcode: int list) a input =
    let getPos arg = match arg with |Position x -> x |Immediate x -> x
    intcode |> List.mapi (fun i item -> if i = getPos a then input else item)
let executeOutput (intcode: int list) a output =
    let getValue arg = match arg with |Position x -> intcode.[x] |Immediate x -> x
    List.append output [getValue a]
let executeJumpTrue (intcode: int list) index (a,b) =
    let value = match a with |Position x -> intcode.[x] |Immediate x -> x
    let pos = match b with |Position x -> intcode.[x] |Immediate x -> x
    if value <> 0 then pos else index + 3
let executeJumpFalse (intcode: int list) index (a,b) =
    let value = match a with |Position x -> intcode.[x] |Immediate x -> x
    let pos = match b with |Position x -> intcode.[x] |Immediate x -> x
    if value = 0 then pos else index + 3
let executeLessThan (intcode: int list) (a,b,c) =
    let getValue arg = match arg with |Position x -> intcode.[x] |Immediate x -> x
    let getPos = match c with |Position x -> x |Immediate x -> x
    intcode |> List.mapi (fun i item -> if i = getPos then (if getValue a < getValue b then 1 else 0) else item)
let executeEquals (intcode: int list) (a,b,c) =
    let getValue arg = match arg with |Position x -> intcode.[x] |Immediate x -> x
    let getPos = match c with |Position x -> x |Immediate x -> x
    intcode |> List.mapi (fun i item -> if i = getPos then (if getValue a = getValue b then 1 else 0) else item)

let rec computeProgram (intcode: int list) (input: int list) output index=
    printfn "computer run"
    match readOpCode intcode index with
    |Finish -> intcode,output
    |Add (a,b,c) -> computeProgram (executeAdd intcode (a,b,c)) input output (index + 4)
    |Mult (a,b,c)-> computeProgram (executeMult intcode (a,b,c)) input output (index + 4)
    |Input a-> computeProgram (executeInput intcode a input.Head) input.Tail output (index + 2)
    |Output a-> computeProgram intcode input (executeOutput intcode a output) (index + 2)
    |JumpTrue (a,b) -> computeProgram intcode input output (executeJumpTrue intcode index (a,b))
    |JumpFalse (a,b) -> computeProgram intcode input output (executeJumpFalse intcode index (a,b))
    |LessThan (a,b,c) -> computeProgram (executeLessThan intcode (a,b,c)) input output (index + 4)
    |Equals (a,b,c) -> computeProgram (executeEquals intcode (a,b,c)) input output (index + 4)
    

let asyncExecuteOutput (intcode: int list) a (messenger:IntcodeVmMessenger) =
    let getValue arg = match arg with |Position x -> intcode.[x] |Immediate x -> x
    do messenger.AddOutput(getValue a)
    messenger

let asyncExecuteInput (intcode: int list) a (messenger:IntcodeVmMessenger)  =
    let getPos arg = match arg with |Position x -> x |Immediate x -> x
    let value = messenger.Receive
    intcode |> List.mapi (fun i item -> if i = getPos a then value else item)


let rec asyncComputeProgram (intcode : int list) (inmessenger: IntcodeVmMessenger) (outmessenger: IntcodeVmMessenger) index =
    match readOpCode intcode index with
    |Finish -> intcode
    |Add (a,b,c) -> asyncComputeProgram (executeAdd intcode (a,b,c)) inmessenger outmessenger (index + 4)
    |Mult (a,b,c)-> asyncComputeProgram (executeMult intcode (a,b,c)) inmessenger outmessenger (index + 4)
    |JumpTrue (a,b) -> asyncComputeProgram intcode inmessenger outmessenger (executeJumpTrue intcode index (a,b))
    |JumpFalse (a,b) -> asyncComputeProgram intcode inmessenger outmessenger (executeJumpFalse intcode index (a,b))
    |LessThan (a,b,c) -> asyncComputeProgram (executeLessThan intcode (a,b,c)) inmessenger outmessenger (index + 4)
    |Equals (a,b,c) -> asyncComputeProgram (executeEquals intcode (a,b,c)) inmessenger outmessenger (index + 4)
    |Output a-> asyncComputeProgram intcode inmessenger (asyncExecuteOutput intcode a outmessenger) (index + 2)
    |Input a-> asyncComputeProgram (asyncExecuteInput intcode a inmessenger) inmessenger outmessenger (index + 2)
    
    