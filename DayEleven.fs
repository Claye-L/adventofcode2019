module DayEleven
open System.Numerics
open AgentProcessings.BigIntAgent
open IntcodeVm
open DayNineIntcodeComputer

let data = "3,8,1005,8,310,1106,0,11,0,0,0,104,1,104,0,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,29,1,2,11,10,1,1101,2,10,2,1008,18,10,2,106,3,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,67,2,105,15,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1001,8,0,93,2,1001,16,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,119,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,141,2,7,17,10,1,1103,16,10,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,102,1,8,170,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1002,8,1,193,1,7,15,10,2,105,13,10,1006,0,92,1006,0,99,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,228,1,3,11,10,1006,0,14,1006,0,71,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,261,2,2,2,10,1006,0,4,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,289,101,1,9,9,1007,9,1049,10,1005,10,15,99,109,632,104,0,104,1,21101,0,387240009756,1,21101,327,0,0,1105,1,431,21101,0,387239486208,1,21102,1,338,0,1106,0,431,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,3224472579,1,1,21101,0,385,0,1106,0,431,21101,0,206253952003,1,21102,396,1,0,1105,1,431,3,10,104,0,104,0,3,10,104,0,104,0,21102,709052072296,1,1,21102,419,1,0,1105,1,431,21102,1,709051962212,1,21102,430,1,0,1106,0,431,99,109,2,21202,-1,1,1,21102,1,40,2,21102,462,1,3,21102,452,1,0,1105,1,495,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,457,458,473,4,0,1001,457,1,457,108,4,457,10,1006,10,489,1101,0,0,457,109,-2,2105,1,0,0,109,4,2102,1,-1,494,1207,-3,0,10,1006,10,512,21101,0,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21101,531,0,0,1105,1,536,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,559,2207,-4,-2,10,1006,10,559,21202,-4,1,-4,1105,1,627,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21102,1,578,0,1105,1,536,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,597,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,619,21201,-1,0,1,21102,1,619,0,106,0,494,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2106,0,0"
let parseProgram (s:string) = 
    let map = s.Split(',') |> Seq.mapi (fun i s -> (bigint i, BigInteger.Parse(s) )) |> Map.ofSeq
    new Intcode(map)


type Direction = |U |D |L |R
let turnLeft d =
    match d with |U -> L |L -> D |D -> R |R -> U
let turnRight d =
    match d with |U -> R |L -> U |D -> L |R -> D
let moveOneInDir (x,y) d =
    match d with 
    |U -> x,y+1
    |D -> x,y-1
    |L -> x-1,y
    |R -> x+1,y
let emptysurface = Map.empty<int*int,bool>

let camera (s:Map<int*int,bool>) coords =
    match s.TryFind coords with
    |Some true -> true
    |_ -> false
type Robot ={
    Surface:Map<int*int,bool>;
    Coords: int*int;
    Dir:Direction;
    PaintedTiles:Set<int*int>
    }

let runRobot (camerames:IntcodeVmMessenger) (movementmes:IntcodeVmMessenger) (Vm:IntcodeVm)=
    let readTile (r:Robot)  = 
        match camera r.Surface r.Coords with
        |true -> camerames.AddOutput 1I
        |false -> camerames.AddOutput 0I
    let updatePos (r:Robot) =
        let paint =  movementmes.Receive = 1I
        let right = movementmes.Receive = 1I
        let (newsurface,nset) = if paint then 
                                    (r.Surface.Add (r.Coords,true), r.PaintedTiles.Add r.Coords) 
                                else 
                                    (r.Surface.Add (r.Coords,false),r.PaintedTiles.Add r.Coords)
        let ndir = if right then turnRight r.Dir else turnLeft r.Dir
        let ncoords = moveOneInDir r.Coords ndir
        {Surface = newsurface; Coords = ncoords; Dir = ndir; PaintedTiles = nset}
    let rec runUntilOutput (vm:IntcodeVm) =
        match vm.NextInstruction() with
        |Output _ -> (vm.Run())
        |_ -> runUntilOutput (vm.Run())
    let rec runPainting (r:Robot) (vm:IntcodeVm) =
        match vm.NextInstruction() with
        |Input _ -> readTile r
                    runPainting r (vm.Run())
        |Output _ -> let nvm = runUntilOutput (vm.Run())
                     let nr = updatePos r
                     runPainting nr nvm
        |Finish -> r,vm
        |_ -> runPainting r (vm.Run())
    //let initRobot = {Surface = emptysurface; Coords = (0,0); Dir = U; PaintedTiles = Set.empty} //part one
    let initRobot = {Surface = emptysurface.Add((0,0),true); Coords = (0,0); Dir = U; PaintedTiles = Set.empty}
    runPainting initRobot Vm
    


let paintSurface =
    let program = parseProgram data
    let camerames = new IntcodeVmMessenger()
    let movementmes = new IntcodeVmMessenger()
    let vm = {Code = program; Index = 0I; Relative = 0I; InMes = camerames; OutMes = movementmes}
    runRobot camerames movementmes vm
