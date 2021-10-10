module Display

open Hai
open Yakuman

let haiName tehai i =
    tehai
    |> List.map (fun hai -> Hai.haiToString hai)
    |> List.map (fun name -> name.[i])
    |> System.String.Concat

let buildKawa sutehai =
    let top =
        if (haiName sutehai 0) = "" then "　"
        else haiName sutehai 0

    let bottom =
        if (haiName sutehai 1) = "" then "　"
        else haiName sutehai 1

    [
        top + "　　　　　";
        bottom + "　　　　　";
        "　　　　　　";
        "　　　　　　";
        "　　　　　　";
        "　　　　　　"
    ]

let displayBoard (info: PlayerHaiInfo) =
    let kawa = buildKawa info.Sutehai

    let board = [
        "　　　　□□□□□□□□□□□□□\n";
        "　　　　□□□□□□□□□□□□□\n";
        "\n";
        "\n";
        "□□　　　　　　　　　　　　　　　　　□□\n";
        "□□　　　　　　　　　　　　　　　　　□□\n";
        "□□　　　　　　　　　　　　　　　　　□□\n";
        "□□　　　　　　　　　　　　　　　　　□□\n";
        "□□　　　　　　　　　　　　　　　　　□□\n";
        "□□　　　　　　　　西　　　　　　　　□□\n";
        "□□　　　　　　　南○北　　　　　　　□□\n";
        "□□　　　　　　　　東　　　　　　　　□□\n";
        "□□　　　　　" + kawa.[0] + "　　　　　　□□\n";
        "□□　　　　　" + kawa.[1] + "　　　　　　□□\n";
        "□□　　　　　" + kawa.[2] + "　　　　　　□□\n";
        "□□　　　　　" + kawa.[3] + "　　　　　　□□\n";
        "□□　　　　　" + kawa.[4] + "　　　　　　□□\n";
        "　　　　　　　" + kawa.[5] + "\n";
        "\n";
        "　　　　" + haiName info.Tehai 0 + "\n";
        "　　　　" + haiName info.Tehai 1
    ]

    printfn "%A" (System.String.Concat board)

    info

let ron (yaku, tehai) =
    printfn "\nロン！\n"

    match yaku with
    | Yakuman.Kokushi13    -> printfn "国士無双十三面待ち\n二倍役満！\n"
    | Yakuman.Churen9      -> printfn "純正九蓮宝燈\n二倍役満！\n"
    | Yakuman.ChinrouTanki -> printfn "清老頭・四暗刻単騎待ち\n三倍役満！\n"
    | Yakuman.Suttan       -> printfn "四暗刻単騎待ち\n二倍役満！\n"

    let haiNameWithBar tehai i =
        (haiName tehai i).ToCharArray()
        |> Array.map (fun char -> char.ToString() + "│")
        |> System.String.Concat

    let aa = [
        "　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　 /＼＿＿_／ヽ\n",
        "　　　 (.｀ヽ(｀＞ 、　　　　　　　　　　　　　　　　　　　　　　／''''''　　　''''''::::::＼\n",
        "　　 　 `'＜｀ゝr'ﾌ＼　　　　　　　　　　　　　　　 　 ＋　　|（●）,　　　､（●）､.:|　＋\n",
        "　 ⊂ｺ二Lﾌ^´　 ノ, ／⌒)　　　　　　　　　　 　 　 　 　　|　　,,,ﾉ(､_, )ヽ､,,　.::::|\n",
        "　　⊂l二L7_　/　-ゝ-')´　　　　　　　　　　　 　　 　　+　|　　 ｀-=ﾆ=- '　.:::::::|　+\n",
        "　　　　　　 ＼_　　､__,.ｲ＼　　　　　　　　　　　＋　　　　 ＼　　 ｀ﾆﾆ´　 .:::／　　　　+\n",
        "　　　　　　　　(T＿_ノ　　 Tヽ　　　　　　　 , -r'⌒!￣ `\":::7ヽ.｀- ､　　　./|\n",
        "　　　　　　　　　ヽ￢.　　　/　ﾉ`ｰ-､ﾍ<ｰ1´|　 ヽ　| :::::::::::::ﾄ、 ＼　(　　./ヽ\n",
        "　　　　　　　　　　＼l__,.／　　 　 　 i　l.ヽ!　|　　　.| ::::::::::::::l ヽ 　 ｀７ｰ.､‐'´ |＼-､\n",
        "　　＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿＿\n",
        "　│" + haiNameWithBar tehai 0 + "\n",
        "　│" + haiNameWithBar tehai 1
    ]

    printfn "%A" (System.String.Concat aa)

    yaku