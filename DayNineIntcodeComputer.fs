module DayNineIntcodeComputer
open AgentProcessings.BigIntAgent

type Argument = |Position of bigint | Immediate of bigint |Relative of bigint
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

type Intcode (inmap :Map<bigint,bigint>) = 
    let map = inmap

    member this.TryFind = map.TryFind
    member this.Add t = new Intcode(map.Add t)

let rec digits x = 
    match x >= 10I with
    |true -> Seq.append (digits (x/10I))  [(x % 10I)]
    |false -> Seq.ofList [x]

let numToArg x=
    match x with
    |0 -> Position
    |1 -> Immediate
    |2 -> Relative
    | _ -> failwith "parsing error when parsing argument"

let valueOrThrow o = match o with |Some x -> x |None -> failwith "no value in memory" 
let readPosition (code:Intcode) pos =
    match code.TryFind pos with 
    |Some x -> x
    |None -> 0I //because the memory is supposed to be infinite and filled with 0, so reading at uninitialized position returns 0
let getValue (code:Intcode) (arg:Argument) (relative:bigint) =
    match arg with
    |Position a -> readPosition code a
    |Immediate a -> a
    |Relative a -> readPosition code (a + relative)
let getPos relative arg =
    match arg with 
    |Position a -> a 
    |Immediate a -> a 
    |Relative a -> a + relative


let read3Args (intcode:Intcode) (index:bigint) (a,b,c)  =
    (numToArg a) (readPosition intcode (index + 1I)),
    (numToArg b) (readPosition intcode (index + 2I)),
    (numToArg c) (readPosition intcode (index + 3I))
let read2Args (intcode:Intcode) (index:bigint) (a,b) =
    (numToArg a) (readPosition intcode (index + 1I)),
    (numToArg b) (readPosition intcode (index + 2I))

let readArg (intcode: Intcode) (index:bigint) a =
    (numToArg a) (readPosition intcode (index + 1I))
    
let intTup2 (a,b) = (int a, int b)
let intTup3 (a,b,c) = (int a,int b, int c)

let readOpCode (intcode: Intcode) (index:bigint) (relative:bigint)=
    let opCode = readPosition intcode index |> digits |> Seq.toList
    let (inst0,inst1) = if opCode.Length >= 2 then opCode.[opCode.Length - 2],opCode.[opCode.Length - 1] else 0I,List.last opCode
    let twodigitinstr = (int inst0, int inst1)
    let padOpCode length = List.append (List.map (fun _->0I) [1.. length - opCode.Length]) opCode
    let instr = 
        match twodigitinstr with 
        |0,1 -> let padded = padOpCode 5
                Add (read3Args intcode index (intTup3 (padded.[2],padded.[1],padded.[0])))
        |0,2 -> let padded = padOpCode 5 
                Mult (read3Args intcode index (intTup3(padded.[2],padded.[1],padded.[0])))
        |9,9 -> Finish
        |0,3 -> let padded = padOpCode 3
                Input (readArg intcode index (int padded.[0]))
        |0,4 -> let padded = padOpCode 3
                Output (readArg intcode index (int padded.[0]))
        |0,5 -> let padded = padOpCode 4
                JumpTrue (read2Args intcode index (intTup2(padded.[1],padded.[0]) ))
        |0,6 -> let padded = padOpCode 4
                JumpFalse (read2Args intcode index (intTup2(padded.[1],padded.[0]) ))
        |0,7 -> let padded = padOpCode 5 
                LessThan (read3Args intcode index (intTup3((padded.[2],padded.[1],padded.[0]))))
        |0,8 -> let padded = padOpCode 5 
                Equals (read3Args intcode index (intTup3(padded.[2],padded.[1],padded.[0])))
        |0,9 -> let padded = padOpCode 3
                IncrementBase (readArg intcode index (int padded.[0]))
        |_,_ -> failwith "Invalid instruction in OpCode"
    instr


let executeAdd (intcode: Intcode) relative (a,b,c) =
    let value = getValue intcode a relative |> (+) <| getValue intcode b relative
    let pos = getPos relative c
    intcode.Add (pos,value)
    
let executeMult (intcode: Intcode) relative (a,b,c) =
    let value = getValue intcode a relative |> (*) <| getValue intcode b relative
    let pos = getPos relative c
    intcode.Add (pos,value)

let executeJumpTrue (intcode: Intcode) relative index (a,b) =
    let value = getValue intcode a relative
    let pos = getValue intcode b relative
    if value <> 0I then pos else index + 3I

let executeJumpFalse (intcode: Intcode) relative index (a,b) =
    let value = getValue intcode a relative
    let pos = getValue intcode b relative
    if value = 0I then pos else index + 3I

let executeLessThan (intcode: Intcode) relative (a,b,c) =
    let valuea = getValue intcode a relative
    let valueb = getValue intcode b relative 
    let pos = getPos relative c
    if(valuea < valueb) then intcode.Add (pos,1I) else intcode.Add (pos,0I)
    
let executeEquals (intcode: Intcode) relative (a,b,c) =
    let valuea = getValue intcode a relative
    let valueb = getValue intcode b relative 
    let pos = getPos relative c
    if(valuea = valueb) then intcode.Add (pos,1I) else intcode.Add (pos,0I)

let asyncExecuteOutput (intcode: Intcode) relative a (messenger:IntcodeVmMessenger) =
    let value = getValue intcode a relative 
    do messenger.AddOutput(value)
    messenger

let asyncExecuteInput (intcode: Intcode) relative a (messenger:IntcodeVmMessenger)  =
    let pos = getPos relative a
    let value = messenger.Receive
    intcode.Add (pos,value)

let executeIncrRelative intcode relative a =
    let value = getValue intcode a relative
    value + relative

let rec asyncComputeProgram (intcode : Intcode) relative (inmessenger: IntcodeVmMessenger) (outmessenger: IntcodeVmMessenger) index =
    match readOpCode intcode index relative with
    |Finish -> intcode
    |Add (a,b,c) -> asyncComputeProgram (executeAdd intcode relative (a,b,c)) relative inmessenger outmessenger (index + 4I)
    |Mult (a,b,c)-> asyncComputeProgram (executeMult intcode relative (a,b,c)) relative inmessenger outmessenger (index + 4I)
    |JumpTrue (a,b) -> asyncComputeProgram intcode relative inmessenger outmessenger (executeJumpTrue intcode relative index (a,b))
    |JumpFalse (a,b) -> asyncComputeProgram intcode relative inmessenger outmessenger (executeJumpFalse intcode relative index (a,b))
    |LessThan (a,b,c) -> asyncComputeProgram (executeLessThan intcode relative (a,b,c)) relative inmessenger outmessenger (index + 4I)
    |Equals (a,b,c) -> asyncComputeProgram (executeEquals intcode relative (a,b,c)) relative inmessenger outmessenger (index + 4I)
    |Output a-> asyncComputeProgram intcode relative inmessenger (asyncExecuteOutput intcode relative a outmessenger) (index + 2I)
    |Input a-> asyncComputeProgram (asyncExecuteInput intcode relative a inmessenger) relative inmessenger outmessenger (index + 2I)
    |IncrementBase a -> asyncComputeProgram intcode (executeIncrRelative intcode relative a) inmessenger outmessenger (index+2I)
    