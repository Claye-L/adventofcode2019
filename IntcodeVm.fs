module IntcodeVm

open DayNineIntcodeComputer



type IntcodeVm =
    {Code: Intcode;
    Index: bigint; 
    Relative: bigint; 
    InMes:AgentProcessings.BigIntAgent.IntcodeVmMessenger; 
    OutMes:AgentProcessings.BigIntAgent.IntcodeVmMessenger}

    member x.NextInstruction() =
         readOpCode x.Code x.Index x.Relative
    
    member x.Run() = //Maybe make this async
        match x.NextInstruction() with
        |Finish -> x
        |Add (a,b,c) -> {x with Code = executeAdd x.Code x.Relative (a,b,c); Index = x.Index + 4I}
        |Mult (a,b,c) -> {x with Code = executeMult x.Code x.Relative (a,b,c); Index = x.Index + 4I}
        |JumpTrue (a,b) -> {x with Index = executeJumpTrue x.Code x.Relative x.Index (a,b)}
        |JumpFalse (a,b) -> {x with Index = executeJumpFalse x.Code x.Relative x.Index (a,b)}
        |LessThan (a,b,c) -> {x with Code = executeLessThan x.Code x.Relative (a,b,c); Index = x.Index + 4I}
        |Equals (a,b,c) -> {x with Code = executeEquals x.Code x.Relative (a,b,c); Index = x.Index + 4I}
        |Output a -> {x with OutMes = asyncExecuteOutput x.Code x.Relative a x.OutMes; Index = x.Index + 2I}
        |Input a -> {x with Code = asyncExecuteInput x.Code x.Relative a x.InMes; Index = x.Index + 2I}
        |IncrementBase a -> {x with Relative = executeIncrRelative x.Code x.Relative a; Index = x.Index + 2I }
