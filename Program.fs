open System
open DayTwelve

let printInConsole (x,y) (offsetx,offsety) =
    Console.SetCursorPosition (offsetx + x * 2, offsety + y)
    Console.Write('*')

[<EntryPoint>]
let main argv =
    TwelveOne
    TwelveTwo
    0 
