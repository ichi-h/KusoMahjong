module Calc

open Yakuman

let calcScore yaku =
    let point =
        match yaku with
        | Yakuman.Kokushi13    -> 32000 * 2
        | Yakuman.Churen9      -> 32000 * 2
        | Yakuman.ChinrouTanki -> 32000 * 3
        | Yakuman.Suttan       -> 32000 * 2

    printfn "\n- 終局 -"
    printfn "You : %A" (25000 - point)
    printfn "CPU1: %A" (25000 + point)
    printfn "CPU2: 25000"
    printfn "CPU2: 25000"


