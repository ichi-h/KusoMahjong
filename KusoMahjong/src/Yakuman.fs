module Yakuman

open Hai
open Util

type Yakuman =
    | Kokushi13
    | Churen9
    | ChinrouTanki
    | Suttan

let allHai info = List.append info.Tehai info.Sutehai

let canBuild info target =
    match target with
    | Kokushi13    -> true
    | Churen9      ->
        match info.Sutehai.[0].HaiType with
        | HaiType.Manzu -> [0; 8]
        | HaiType.Pinzu -> [9; 17]
        | HaiType.Sohzu -> [18; 26]
        | _             -> []
        |> List.map (fun i -> Hai.numToHai i)
        |> List.map (fun hai -> count (allHai info) hai)
        |> List.filter (fun c -> c < 2)
        |> List.length = 2
    | ChinrouTanki ->
        let ichiKyu = [0; 8; 9; 17; 18; 26]

        if (count ichiKyu (int info.Sutehai.[0].HaiNumber)) <> 1 then false
        else
            [0; 8; 9; 17; 18; 26]
            |> List.map (fun i -> Hai.numToHai i)
            |> List.map (fun hai -> count (allHai info) hai)
            |> List.filter (fun c -> c < 2)
            |> List.length > 4
    | Suttan       -> true

let decideYaku info =
    match info.Sutehai.[0].HaiType with
    | HaiType.Fon | HaiType.Sangen -> Yakuman.Kokushi13
    | _ ->
        if   canBuild info Yakuman.Churen9 then Yakuman.Churen9
        elif canBuild info Yakuman.ChinrouTanki then Yakuman.ChinrouTanki
        else Yakuman.Suttan

let rec addHai (lst: Hai list) (hai, c) =
    if lst.Length = 13 || c = 3 then lst
    else addHai (List.append lst [hai]) (hai, c+1)

let buildYakuman info =
    let yaku = decideYaku info
    let sutehai = info.Sutehai.[0]

    let tehai =
        match yaku with
        | Kokushi13 ->
            [0; 8; 9; 17; 18; 26; 27; 28; 29; 30; 31; 32; 33]
            |> List.map (fun i -> Hai.numToHai i)
        | Churen9 ->
            [0; 0; 0; 1; 2; 3; 4; 5; 6; 7; 8; 8; 8]
            |> List.map (fun i ->
                match sutehai.HaiType with
                | HaiType.Manzu -> i
                | HaiType.Pinzu -> i + 9
                | HaiType.Sohzu -> i + 18
                | _ -> raise (System.Exception("字牌で九蓮はできんぞ")))
                |> List.map (fun i -> Hai.numToHai i)
        | ChinrouTanki ->
            let ichiKyu = [0; 8; 9; 17; 18; 26] |> List.map (fun i -> Hai.numToHai i)

            let unusable =
                ichiKyu
                |> List.map (fun hai ->
                    if hai = sutehai then 4
                    else count (allHai info) hai)

            List.zip ichiKyu unusable
            |> List.fold (fun acc (hai, c) ->
                if c <= 1 then addHai acc (hai, 0)
                else acc
            ) [sutehai]
        | Suttan ->
            let hais = [0..33] |> List.map (fun i -> Hai.numToHai i)
            let unusable =
                hais
                |> List.map (fun hai ->
                    if hai = sutehai then 4
                    else count (allHai info) hai)

            List.zip hais unusable
            |> List.fold (fun acc (hai, c) ->
                if c <= 1 then addHai acc (hai, 0)
                else acc
            ) [sutehai]

        |> List.append [sutehai]

    (yaku, tehai)