// Learn more about F# at http://fsharp.org

open System
open Four
open AgentProcessing
open DayEight


let postmessageasync (messenger : IntcodeVmMessenger) = async {
    Threading.Thread.Sleep(1000)
    messenger.AddOutput (20)
}

[<EntryPoint>]
let main argv =
    //printfn "%d" OneOne.OneTwo
    //printfn "%s"  TwoOne.TwoOne
    //printfn "%s"  ThreeOne.ThreeOne
    //printfn "%d" Four.FourTwo
    //let five = Five.FiveTwo
    //let l = snd five
    //printfn "%s"  (string ((snd five).Head))
    //IntCodeComputerTests.tests
    //printfn "%d" Six.SixTwo
    //let (output,perm) = Seven.Seven
    //printfn "%d %A"  perm output
    //let messenger = new IntcodeVmMessenger()
    //messenger.AddInput (Output 0)
    //printfn "%d" messenger.Receive
    //messenger.AddInput (Output 5)
    //messenger.AddInput(Output 10)
    //printfn "%d" messenger.Receive
    //printfn "%d" messenger.Receive
    //Async.Start (postmessageasync messenger)
    //printfn "%d" messenger.Receive
    //printfn "%A" Seven.Seven2
    //printfn "%d" EightOne
    EightTwo
    0 
