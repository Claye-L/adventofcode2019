module DayFourteen

open System
type Element = |Ore |Fuel |Chem of string
type Reaction = {Reagents:(int*Element) list; Product:(int*Element)}
type ReactionMap = Map<Element, Reaction>
type SumMap = Map<Element,int>

let getProdOfreaction (r:Reaction) = snd r.Product

let testData0 = "10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"

let parseElement (s:string) =  
    let quant = s.Trim().Split(' ').[0] |> int
    let elem = match s.Trim().Split(' ').[1] with
                |"ORE" -> Ore
                |"FUEL" -> Fuel
                |a -> Chem a
    quant,elem
let parseLine (line:string) = 
    let reagentString = line.Split("=>").[0]
    let productString = line.Split("=>").[1]
    let reagents = reagentString.Split(',') |> Array.map parseElement
    let products = parseElement productString
    {Reagents= Array.toList reagents; Product = products}

let parseData (data:string) = 
    data.Split(Environment.NewLine) |> Array.map parseLine |> Array.toList

let makeReactionMap (rs:Reaction list) =
    List.map (fun r -> getProdOfreaction r,r) rs  |> Map.ofList 

let rec traverseTree m

let Fourteen =
    let reactions = parseData testData0
    let tree = makeReactionMap reactions
    printfn "%A" tree