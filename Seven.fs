module Seven
open IntCodeComputer
open System.Threading
open AgentProcessings.AgentProcessing

let distrib e L =
    let rec aux pre post = 
        seq {
            match post with
            | [] -> yield (L @ [e])
            | h::t -> yield (List.rev pre @ [e] @ post)
                      yield! aux (h::pre) t 
        }
    aux [] L

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)

let runThroughAmplifier ampCode (sequence: int list) =
    let (_,outa) = computeProgram ampCode [sequence.[0];0] [] 0
    let (_,outb) = computeProgram ampCode [sequence.[1];List.last outa] [] 0
    let (_,outc) = computeProgram ampCode [sequence.[2];List.last outb] [] 0
    let (_,outd) = computeProgram ampCode [sequence.[3];List.last outc] [] 0
    let (_,oute) = computeProgram ampCode [sequence.[4];List.last outd] [] 0
    List.last oute


let rawData = "3,8,1001,8,10,8,105,1,0,0,21,42,55,64,85,98,179,260,341,422,99999,3,9,101,2,9,9,102,5,9,9,1001,9,2,9,1002,9,5,9,4,9,99,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,3,9,9,4,9,99,3,9,1002,9,4,9,101,3,9,9,102,5,9,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99"
let testData = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
let testData1 = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
let testData2 = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
let testData3 = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
let testData4 = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
let data = rawData.Split(',') |> Seq.map int |> Seq.toList


//let seven1 =
//    perms [0;1;2;3;4] |> Seq.map (fun s -> s, runThroughAmplifier data s) |> Seq.maxBy (fun (a,b) -> b)
    

let runThroughAmpFeedback ampCode (sequence: int list) input =
    let (_,outa) = computeProgram ampCode (sequence.[0]::input) [] 0
    let (_,outb) = computeProgram ampCode (sequence.[1]::outa) [] 0
    let (_,outc) = computeProgram ampCode (sequence.[2]::outb) [] 0
    let (_,outd) = computeProgram ampCode (sequence.[3]::outc) [] 0
    let (_,oute) = computeProgram ampCode (sequence.[4]::outd) [] 0
    List.last oute
    
let runVmAsync intcode inmessenger outmessenger = async {
    asyncComputeProgram intcode inmessenger outmessenger 0
}

let runFeedbackAsync data (phases: int list) =
    let mesab = new IntcodeVmMessenger()
    let mesbc = new IntcodeVmMessenger()
    let mescd = new IntcodeVmMessenger()
    let mesde = new IntcodeVmMessenger()
    let mesea = new IntcodeVmMessenger()
    let messengers = [mesea;mesab;mesbc;mescd;mesde;]
    let messeng = Seq.iter2 (fun  (messenger:IntcodeVmMessenger) phase-> 
                                        messenger.AddOutput(phase) |> ignore
                                        ) messengers phases
    messeng
    mesea.AddOutput(0) //initial conditions
    Async.Start (runVmAsync data mesea mesab)
    Async.Start (runVmAsync data mesab mesbc)
    Async.Start (runVmAsync data mesbc mescd)
    Async.Start (runVmAsync data mescd mesde)
    Async.RunSynchronously(runVmAsync data mesde mesea)
    mesea.Receive

let Seven2 =
    perms [5;6;7;8;9] |> Seq.map(fun x -> x ,runFeedbackAsync data x) |> Seq.maxBy snd