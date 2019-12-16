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
    let data = parseData rawData 
    let (point,visible) = List.map (fun x -> x, visiblePoints x data) data |> List.maxBy (snd >> Set.count )
    printfn "%A" point
    printfn "%d" <| Set.count visible

let optimalPoint = (20,20)

[<StructuralEquality; CustomComparison>]
type AngleCosine =
    |R of float * (int*int)
    |L of float * (int*int)
    
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

let vecWithPoint (a,b) (x,y) =   x - a, (y - b) * -1
    
let cosAngle (x,y) = 
    let right =  x >= 0
    let (a,b) = if right then 0,1 else 0,-1
    let dotproduct = a * x + b * y
    let normproduct = float 1 + ( (float >>  Math.Sqrt) (x * x + y * y))
    let cos = float dotproduct / (normproduct)
    (if right then R else L)  (cos, (x,y))

let scanClockwise roids =
    let rec takeFirstLayer items (rejects: AngleCosine list) (s: Set<AngleCosine>) = seq {
        match items with
        |a :: tail -> match s.Contains a with
                      |false -> yield a
                                yield! takeFirstLayer tail rejects (s.Add a)
                      |true -> yield! takeFirstLayer tail (a::rejects) s
        |[] -> match rejects.Length > 0 with
               |true -> yield! takeFirstLayer (List.rev rejects) [] Set.empty
               |false -> ()
        }
    takeFirstLayer roids [] Set.empty
let associate x f =
    x, f x
let TenTwo =
    let data = parseData rawData |> List.filter ((<>) optimalPoint)
    let vec = vecWithPoint optimalPoint
    let angles = List.map (vec >> cosAngle) data |> List.sort
    let orderedRoids = scanClockwise angles
    orderedRoids |> Seq.iteri (fun index angle -> printfn "index %d for ast %A" (index + 1) angle)