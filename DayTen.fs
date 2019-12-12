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