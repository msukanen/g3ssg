[<AutoOpen>]
module rec AstroMeasurement
type distance =
    | AU of decimal
    | LY of decimal
    | PC of decimal
    | Km of decimal
    static member (+) (a:distance, b:distance) =
        match a with
        | AU a -> AU (a + distance.raw (distance.toAU b))
        | LY a -> LY (a + distance.raw (distance.toLY b))
        | PC a -> PC (a + distance.raw (distance.toPC b))
        | Km a -> Km (a + distance.raw (distance.toKm b))
    static member (*) (a:distance, b:distance) =
        match a with
        | AU a -> AU (a * distance.raw (distance.toAU b))
        | LY a -> LY (a * distance.raw (distance.toLY b))
        | PC a -> PC (a * distance.raw (distance.toPC b))
        | Km a -> Km (a * distance.raw (distance.toKm b))
    static member (*) (a:decimal, b:distance) =
        match b with
        | AU x -> AU (a * x)
        | LY x -> LY (a * x)
        | PC x -> PC (a * x)
        | Km x -> Km (a * x)
    static member (*) (a:distance, b:decimal) = b*a
    static member private raw v =
        match v with
        | AU a -> a 
        | LY a -> a
        | PC a -> a
        | Km a -> a
    static member toLY (a:distance) =
        match a with
        | Km x -> Km x |> distance.toAU |> distance.toLY
        | AU x -> x / 63241.077088071m |> LY
        | PC x -> 3.2615637769m * x |> LY
        | _    -> a
    static member toAU (a:distance) =
        match a with
        | Km x -> x / 149597870.691m |> AU
        | LY x -> x * 63241.077088071m |> AU
        | PC x -> PC x |> distance.toLY |> distance.toAU
        | _    -> a
    static member toPC (a:distance) =
        match a with
        | Km x -> Km x |> distance.toAU |> distance.toLY |> distance.toPC
        | AU x -> AU x |> distance.toLY |> distance.toPC
        | LY x -> x / 3.2615637769m |> PC
        | _    -> a
    static member toKm (a:distance) =
        match a with
        | AU x -> x * 149597870.691m |> Km
        | LY x -> LY x |> distance.toAU |> distance.toKm
        | PC x -> PC x |> distance.toLY |> distance.toAU
        | _    -> a
    member private m.fmtF =
        lazy ((match m with
               | AU a -> $"%0.3f{a}"
               | LY a -> $"%0.3f{a}"
               | PC a -> $"%0.3f{a}"
               | Km a -> $"%0.3f{a}").TrimEnd[|'0'|])
    member private m.unitF =
        lazy (match m with
              | AU _ -> " AU"
              | LY _ -> " ly"
              | PC _ -> " pc"
              | Km _ -> "km")
    override m.ToString() = m.fmtF.Force() + m.unitF.Force()
    member m.ToString(withUnit:bool) =
        match withUnit with
        | false -> m.fmtF.Force()
        | _     -> m.ToString()

let Km_Z = Km 0m
let AU_Z = AU 0m
let LY_Z = LY 0m
let PC_Z = PC 0m
let LZY0 = lazy (0)
