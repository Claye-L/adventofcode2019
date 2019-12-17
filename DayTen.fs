module DayTen

open System

let rawData = "#.#....#.#......#.....#......####.
#....#....##...#..#..##....#.##..#
#.#..#....#..#....##...###......##
...........##..##..##.####.#......
...##..##....##.#.....#.##....#..#
..##.....#..#.......#.#.........##
...###..##.###.#..................
.##...###.#.#.......#.#...##..#.#.
...#...##....#....##.#.....#...#.#
..##........#.#...#..#...##...##..
..#.##.......#..#......#.....##..#
....###..#..#...###...#.###...#.##
..#........#....#.....##.....#.#.#
...#....#.....#..#...###........#.
.##...#........#.#...#...##.......
.#....#.#.#.#.....#...........#...
.......###.##...#..#.#....#..##..#
#..#..###.#.......##....##.#..#...
..##...#.#.#........##..#..#.#..#.
.#.##..#.......#.#.#.........##.##
...#.#.....#.#....###.#.........#.
.#..#.##...#......#......#..##....
.##....#.#......##...#....#.##..#.
#..#..#..#...........#......##...#
#....##...#......#.###.#..#.#...#.
#......#.#.#.#....###..##.##...##.
......#.......#.#.#.#...#...##....
....##..#.....#.......#....#...#..
.#........#....#...#.#..#....#....
.#.##.##..##.#.#####..........##..
..####...##.#.....##.............#
....##......#.#..#....###....##...
......#..#.#####.#................
.#....#.#..#.###....##.......##.#."

let testData = ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##"

let parseData (s:string) =
    s.Split(Environment.NewLine) 
    |> Seq.map (fun l -> Seq.indexed l |> Seq.filter (fun (i,c) -> c = '#'))
    |> Seq.indexed 
    |> Seq.map (fun (line,l) -> Seq.map (fun (col,_) -> line,col) l)
    |> Seq.concat
    |> List.ofSeq
type Angle =
    |UR of double
    |UL of double
    |BR of double
    |BL of double
let angle (a,b) (x,y) =
    let right = a > x
    let top = b > y
    let direction = 
        match right,top with
        |true,true -> UR
        |true,false -> BR
        |false,true -> UL
        |false,false -> BL
    direction <| (double <| y - b) / (double <| x - a)

let visiblePoints point points =
    Seq.filter (fun x -> x <> point) points |> Seq.map (fun p -> angle point p) 
    |> Set.ofSeq
let TenOne =
    let data = parseData testData
    let (point,visible) = List.map (fun x -> x, visiblePoints x data) data |> List.maxBy (snd >> Set.count )
    printfn "%A" point
    printfn "%d" <| Set.count visible

let optimalPoint = (20,20)

[<CustomEquality; CustomComparison>]
type AngleCosine =
    |R of float * (int*int)
    |L of float * (int*int)
    member x.cos =
        match x with
        |R (a,_) -> a
        |L (a,_) -> a
    member x.desc =
        match x with
        |R (a,_) -> 0,a
        |L (a,_) -> 1,a
    override x.Equals(yobj) =
        match yobj  with
            | :? AngleCosine as y -> match x,y with
                                     |R (a,_), R (b,_) -> a = b
                                     |L (a,_), L (b,_) -> a = b
                                     |_ -> false
            | _ -> failwith "unauthorized comparison of AngleCosine object"
    interface System.IComparable with
        member x.CompareTo yobj =
            let comp x y =
                match x,y with
                |R (a, _) , R (b , _) -> compare b a
                |R _, L _ -> -1
                |L _, R _ -> 1
                |L (a,_), L (b,_) -> compare b a
            match yobj  with
            | :? AngleCosine as y -> comp x y
            | _ -> failwith "unauthorized comparison of AngleCosine object"

let vecWithPoint (a,b) (x,y) = (x - a), (y - b)
    
let cosAngle (x,y) coords = 
    let right =  x >= 0
    let (a,b) = if right then 0,-1 else 0,1
    let dotproduct = a * x + b * y
    let normproduct = ( Math.Sqrt( float (x * x + y * y)))
    let cos = float dotproduct / (normproduct)
    let fac = float (if right then 1 else -1)
    (if right then R else L)  (cos * -1.0, coords)
let distance (ac:AngleCosine) p =
    match ac with 
    | R (_,f) -> let (a,b) = vecWithPoint f p 
                 (float >>  Math.Sqrt) (a*a + b*b)
    |L (_,f) -> let (a,b) = vecWithPoint f p 
                (float >>  Math.Sqrt) (a*a + b*b)

//let scanClockwise roids refPoint =
//    let rec takeFirstLayer items (rejects: AngleCosine list) (m: Map<AngleCosine, float>) = seq {
//        match items with
//        |a :: tail -> match m.TryFind a with
//                      |None ->  yield! takeFirstLayer tail rejects (m.Add (a,(distance a refPoint)))
//                      |Some b ->   if distance a refPoint < b then 
//                                       yield! takeFirstLayer tail (m.[a]::rejects) (m.Add (a,(distance a refPoint))) 
//                                    else 
//                                        yield! takeFirstLayer tail (a::rejects) m
//        |[] -> match rejects.Length > 0 with
//               |true -> yield! takeFirstLayer (List.rev rejects) [] Set.empty
//               |false -> ()
//        }
//    takeFirstLayer roids [] Map.empty

let takeFirstFromEach (angles:AngleCosine list list )= 
    let rec f input = seq {
        match List.isEmpty input with 
        |true -> ()
        |false ->  yield! Seq.map (List.head) input
                   yield! f ( Seq.map List.tail input |> Seq.filter (List.isEmpty >> not) |> Seq.toList)
    }
    f angles
let swap (a,b) = b,a
let TenTwo =
    let point = optimalPoint
    let data = parseData rawData|> List.map swap |> List.filter ((<>) point ) 
    let vec = vecWithPoint point
    let angles = List.map (fun x -> cosAngle (vec x) x ) data 
    let angleGroup = angles |> List.groupBy (fun x -> x.desc) |> List.sortBy (fun x -> x)
    let sortedGroup = angleGroup |> List.map (fun (_,l) -> List.sortBy (fun x -> distance x point) l)
    takeFirstFromEach sortedGroup |> Seq.iteri (fun index angle -> printfn "index %d for ast %A" (index + 1) angle)
    //let orderedRoids = scanClockwise angles
    //orderedRoids |> 
   