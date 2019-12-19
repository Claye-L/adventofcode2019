open System
open DayTwelve
open DayThirteen

let printInConsole (x,y) (offsetx,offsety) =
    Console.SetCursorPosition (offsetx + x * 2, offsety + y)
    Console.Write('*')

[<EntryPoint>]
let main argv =
    DayFourteen.Fourteen
    0 
