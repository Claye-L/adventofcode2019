module DayTwelve

open System

type Vec3 = {x:int;y:int;z:int}
type Moon = {Position:Vec3;Velocity:Vec3}

let vec3 x y z = {x=x;y=y;z=z;}
let puzzleInput = [ 
    vec3 10 15 7
    vec3 15 10 0
    vec3 20 12 3
    vec3 0 -3 13
]
let testData = [
    vec3 -1 0 2;
    vec3 2 -10 -7;
    vec3 4 -8 8;
    vec3 3 5 -1;
]
let vec0 = {x=0;y=0;z=0}
let setNullVelocity v = {Position =v; Velocity = vec0}

let makeMoons (data: Vec3 list) = List.map setNullVelocity data

let moonWithVel m v = {m with Velocity = v}
let getVelChange p2 p1 =
    {x = compare p1.x p2.x; y = compare p1.y p2.y; z = compare p1.z p2.z}

let addVelocities v1 v2 =
    {x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z}

let applyVelocity m =
    let npos = addVelocities m.Position m.Velocity
    {m with Position = npos}
let computeStep (moons:Moon list) =
    let pairsMoon = List.map (fun x -> x,moons) moons
    let updatedVelocities = List.map (fun (m,l) -> Seq.map (fun x -> getVelChange m.Position x.Position) l 
                                                   |> Seq.fold addVelocities m.Velocity 
                                                   |> moonWithVel m) pairsMoon
    List.map applyVelocity updatedVelocities
let rec computeSteps moons s =
    match s with 
    |0 -> moons
    |a -> computeSteps (computeStep moons) (a-1)
let absSum v = Math.Abs(v.x) + Math.Abs(v.y) + Math.Abs(v.z) 
let energy moon =
    absSum moon.Position * absSum moon.Velocity
let TwelveOne =
    let moons = makeMoons puzzleInput
    printfn "%A" moons
    let step = computeSteps moons
    List.map energy (step 1000) |> Seq.sum |> printfn "%d"
let revAssociate f x = f x,x
    
let computeStepAxis (coords: (int*int) list) =
    List.map (fun (pos,vel) -> Seq.map (fun (p,v) -> compare p pos) coords |> Seq.fold (+) vel |> revAssociate ((+) pos) ) coords

let rec gcd x y = if y = 0 then abs x else gcd y (x % y)
 
let lcm x y = x * y / (gcd x y)

let TwelveTwo =
    let moons = makeMoons testData
    let initialStateX = List.map (fun x -> x.Position.x,0) moons
    let initialStateY = List.map (fun x -> x.Position.y,0) moons
    let initialStateZ = List.map (fun x -> x.Position.z,0) moons
    let isBackToStart initialState (s:seq<int*int>) = Seq.zip initialState s |> Seq.forall (fun (a,b) -> snd a = snd b)
    let step1 = computeStepAxis initialStateX
    printfn "%A" initialStateX
    let cycleForAxis initialState =
        Seq.initInfinite (id) 
        |> Seq.scan (fun ms i -> computeStepAxis ms) initialState
        |> Seq.skip 1
        |> Seq.takeWhile (fun x -> not <| isBackToStart initialState x)
        //|> Seq.iter (fun x -> printfn "%A" x)
        |> Seq.length
    let lx = cycleForAxis initialStateX
    let ly = cycleForAxis initialStateY
    let lz = cycleForAxis initialStateZ
    printfn "%d %d %d" lx ly lz
    //printfn "%A" step1
