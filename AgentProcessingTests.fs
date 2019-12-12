namespace AgentProcessings
module AgentProcessing =

    type IntCodeMsg =
        |Output of int
        |Input of AsyncReplyChannel<int>

    type IntcodeVmMessenger () =
        let inputAgent = MailboxProcessor.Start(fun inbox -> 
    
            let rec inputLoop ((storedOutput: int list), (storedChannel: AsyncReplyChannel<int> list)) = async {
                let replyIfPossible (output: int list) (channels: AsyncReplyChannel<int> list) =
                    match output,channels with
                        |[],_ 
                        |_,[] -> output,channels
                        |a :: tail,channel::cs -> 
                                                //printfn "Inputted %d" a
                                                channel.Reply(a)
                                                tail,cs
                let! msg = inbox.Receive()
                match msg with 
                |Output (b) ->  //printfn " Outputted %d" b
                                return! inputLoop (replyIfPossible (List.append storedOutput [b]) storedChannel)
                |Input channel -> return! inputLoop (replyIfPossible storedOutput (List.append storedChannel [channel]))
            }
            inputLoop ([],[])
        )
        member this.AddOutput i = inputAgent.Post (Output i)
        member this.Receive = inputAgent.PostAndReply(fun rep -> Input rep)

module BigIntAgent =

    type IntCodeMsg =
        |Output of bigint
        |Input of AsyncReplyChannel<bigint>

    type IntcodeVmMessenger () =
        let inputAgent = MailboxProcessor.Start(fun inbox -> 
    
            let rec inputLoop ((storedOutput: bigint list), (storedChannel: AsyncReplyChannel<bigint> list)) = async {
                let replyIfPossible (output: bigint list) (channels: AsyncReplyChannel<bigint> list) =
                    match output,channels with
                        |[],_ 
                        |_,[] -> output,channels
                        |a :: tail,channel::cs -> 
                                                //printfn "Inputted %d" a
                                                channel.Reply(a)
                                                tail,cs
                let! msg = inbox.Receive()
                match msg with 
                |Output (b) ->  //printfn " Outputted %d" b
                                return! inputLoop (replyIfPossible (List.append storedOutput [b]) storedChannel)
                |Input channel -> return! inputLoop (replyIfPossible storedOutput (List.append storedChannel [channel]))
            }
            inputLoop ([],[])
        )
        member this.AddOutput i = inputAgent.Post (Output i)
        member this.Receive = inputAgent.PostAndReply(fun rep -> Input rep)