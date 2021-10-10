module DealHais

open Hai
open Util

type TenhoChecker (tehai: Hai list) =
    let tehai = tehai

    member public this.check = this.checkRule && this.checkMentsu && this.checkKantsu && this.checkChitoitsu && this.checkKokushi

    member private this.checkKokushi = 
        [0; 8; 9; 17; 18; 26; 27; 28; 29; 30; 31; 32; 33]
        |> List.filter (fun num -> 0 < count this.nums num)
        |> List.length < 12

    member private this.checkChitoitsu =
        let toitsu    = this.nums |> List.filter (fun num -> (count this.nums num = 2))
        let notToitsu = this.nums |> List.filter (fun num -> (count this.nums num <> 2))
        
        not (toitsu.Length = 12 || toitsu.Length = 10 && (count notToitsu notToitsu.Head) = 3)

    member private this.checkMentsu =
        [0..4]
        |> List.map (fun i -> enum<HaiType>(i))
        |> List.fold (fun x t ->
            let nums = 
                tehai
                |> List.map (fun hai -> (int hai.HaiNumber))
                |> List.filter (fun num ->
                    match t with
                    | HaiType.Manzu  ->  0 <= num && num <  9
                    | HaiType.Pinzu  ->  9 <= num && num < 18
                    | HaiType.Sohzu  -> 18 <= num && num < 27
                    | HaiType.Fon    -> 27 <= num && num < 31
                    | HaiType.Sangen -> 31 <= num && num < 34)
            nums
            |> List.fold (fun (y: int) num ->
                let isAnco target = (count nums target) = 3
                let isJuntsu target = 1 <= count nums (target + 1) && 1 <= count nums (target + 2)
        
                if   isAnco num && isJuntsu num then y + 2
                elif isAnco num || isJuntsu num then y + 1
                else y
            ) x
        ) 0
        |> (<) 3

    member private this.checkKantsu =
        tehai
        |> List.filter (fun hai -> (count tehai hai) = 4)
        |> List.length = 0

    member private this.checkRule =
        this.nums
        |> List.filter (fun num -> 3 < count this.nums num)
        |> List.length = 0

    member private this.nums =
        tehai
        |> List.map (fun hai -> int hai.HaiNumber)
        |> List.sort
        
 
let rec dealHais () =
    let rnd = System.Random()

    let tehai =
        [for _ in 0..12 -> rnd.Next 136]
        |> List.map (fun v ->
            if     0 <= v && v <  36 then HaiType.Manzu
            elif  36 <= v && v <  72 then HaiType.Pinzu
            elif  72 <= v && v < 108 then HaiType.Sohzu
            elif 108 <= v && v < 124 then HaiType.Fon
            else HaiType.Sangen)
        |> List.map (fun t ->
            match t with
            | HaiType.Manzu  -> { HaiType = t; HaiNumber = enum<HaiNumber>( 0 + rnd.Next 9) }
            | HaiType.Pinzu  -> { HaiType = t; HaiNumber = enum<HaiNumber>( 9 + rnd.Next 9) }
            | HaiType.Sohzu  -> { HaiType = t; HaiNumber = enum<HaiNumber>(18 + rnd.Next 9) }
            | HaiType.Fon    -> { HaiType = t; HaiNumber = enum<HaiNumber>(27 + rnd.Next 4) }
            | HaiType.Sangen -> { HaiType = t; HaiNumber = enum<HaiNumber>(31 + rnd.Next 3) })
        |> List.sort

    if TenhoChecker(tehai).check then { Tehai = tehai; Sutehai = [] }
    else dealHais ()
