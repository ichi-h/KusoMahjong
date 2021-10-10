module Change

open Hai
open Util

let rec tsumo (info: PlayerHaiInfo) =
    let newHai = Hai.createRandHai

    if 4 < count (List.append info.Tehai info.Sutehai) newHai then tsumo info
    else
        printfn "ツモった牌: %A" (Hai.haiToString newHai)
        newHai

let isCorrect input =
    try
        let i = int input
        i < 14
    with
    | :? System.FormatException -> false

let rec selectHai () =
    printf "捨牌: "

    let input = stdin.ReadLine()

    if not (isCorrect input) then selectHai ()
    else int input

let changeHai (info: PlayerHaiInfo) =
    let curTehai = List.append info.Tehai [tsumo info]

    let target = selectHai ()

    let newSutehai = List.append info.Sutehai [curTehai.[target]]

    let newTehai =
        List.zip curTehai [0..curTehai.Length-1]
        |> List.filter (fun (_, i) -> i <> target)
        |> List.map (fun (hai, _) -> hai)
        |> List.sort

    { Tehai = newTehai; Sutehai = newSutehai }
    