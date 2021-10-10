// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open DealHais
open Display
open Change
open Yakuman
open Calc

let start () =
    dealHais ()
    |> displayBoard
    |> changeHai
    |> displayBoard
    |> buildYakuman
    |> ron
    |> calcScore

[<EntryPoint>]
let main argv =
    start ()
    
    0 // return an integer exit code
