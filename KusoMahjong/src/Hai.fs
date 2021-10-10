namespace Hai

type HaiType =
    | Manzu = 0
    | Pinzu = 1
    | Sohzu = 2
    | Fon = 3
    | Sangen = 4

type HaiNumber =
    | IWan    =  0
    | RyanWan =  1
    | SanWan  =  2
    | SuWan   =  3
    | UWan    =  4
    | RoWan   =  5
    | ChiWan  =  6
    | PaWan   =  7
    | KyuWan  =  8
    | IPin    =  9
    | RyanPin = 10
    | SanPin  = 11
    | SuPin   = 12
    | UPin    = 13
    | RoPin   = 14
    | ChiPin  = 15
    | PaPin   = 16
    | KyuPin  = 17
    | ISo     = 18
    | RyanSo  = 19
    | SanSo   = 20
    | SuSo    = 21
    | USo     = 22
    | RoSo    = 23
    | ChiSo   = 24
    | PaSo    = 25
    | KyuSo   = 26
    | East    = 27
    | South   = 28
    | West    = 29
    | North   = 30
    | White   = 31
    | Green   = 32
    | Red     = 33

type Hai =
    {
        HaiType: HaiType
        HaiNumber: HaiNumber;
    }

type PlayerHaiInfo =
    {
        Tehai: Hai list
        Sutehai: Hai list;
    }

module Hai =

    let haiToString (hai: Hai) =
        let num = int hai.HaiNumber
        let typ = hai.HaiType

        let head =
            if num < 27 then
                match num % 9 with
                | 0 -> "一"
                | 1 -> "二"
                | 2 -> "三"
                | 3 -> "四"
                | 4 -> "五"
                | 5 -> "六"
                | 6 -> "七"
                | 7 -> "八"
                | 8 -> "九"
            else
                match num with
                | 27 -> "東"
                | 28 -> "南" 
                | 29 -> "西"
                | 30 -> "北"
                | 31 -> "白"
                | 32 -> "發"
                | 33 -> "中"

        let tail =
            match typ with
            | HaiType.Manzu -> "萬"
            | HaiType.Pinzu -> "筒"
            | HaiType.Sohzu -> "索"
            | _             -> "　"

        head + tail

    let numToHai num =
        if    0 <= num && num <  9 then { HaiType = enum<HaiType>(0); HaiNumber = enum<HaiNumber>(num) }
        elif  9 <= num && num < 18 then { HaiType = enum<HaiType>(1); HaiNumber = enum<HaiNumber>(num) }
        elif 18 <= num && num < 27 then { HaiType = enum<HaiType>(2); HaiNumber = enum<HaiNumber>(num) }
        elif 27 <= num && num < 31 then { HaiType = enum<HaiType>(3); HaiNumber = enum<HaiNumber>(num) }
        elif 31 <= num && num < 34 then { HaiType = enum<HaiType>(4); HaiNumber = enum<HaiNumber>(num) }
        else raise (System.Exception("数字は0-33の間で記述しろや: " + num.ToString()))

    let createRandHai =
        let rnd = System.Random()

        let v = rnd.Next 136
        
        let t =
            if     0 <= v && v <  36 then HaiType.Manzu
            elif  36 <= v && v <  72 then HaiType.Pinzu
            elif  72 <= v && v < 108 then HaiType.Sohzu
            elif 108 <= v && v < 124 then HaiType.Fon
            else HaiType.Sangen

        match t with
        | HaiType.Manzu  -> { HaiType = t; HaiNumber = enum<HaiNumber>( 0 + rnd.Next 9) }
        | HaiType.Pinzu  -> { HaiType = t; HaiNumber = enum<HaiNumber>( 9 + rnd.Next 9) }
        | HaiType.Sohzu  -> { HaiType = t; HaiNumber = enum<HaiNumber>(18 + rnd.Next 9) }
        | HaiType.Fon    -> { HaiType = t; HaiNumber = enum<HaiNumber>(27 + rnd.Next 4) }
        | HaiType.Sangen -> { HaiType = t; HaiNumber = enum<HaiNumber>(31 + rnd.Next 3) }