[<AutoOpen>]
module rec AstroMeasurement

open System

[<CustomComparison; CustomEquality>]
type distance =
    | AU of decimal
    | LY of decimal
    | PC of decimal
    | Km of decimal
    override x.Equals o =
        match o with
        | :? distance as y ->
            match x with
            | AU z -> z = distance.exAU y
            | LY z -> z = distance.exLY y
            | PC z -> z = distance.exPC y
            | Km z -> z = distance.exKm y
        | _ -> false
    override x.GetHashCode() =
        match x with
        | AU z -> z.GetHashCode()
        | LY z -> z.GetHashCode() + 1
        | PC z -> z.GetHashCode() + 2
        | Km z -> z.GetHashCode() + 3
    interface IComparable with
        member x.CompareTo o =
            match o with
            | :? distance as y ->
                match x with
                | AU z -> compare z (distance.exAU y)
                | LY z -> compare z (distance.exLY y)
                | PC z -> compare z (distance.exPC y)
                | Km z -> compare z (distance.exKm y)
            | _ -> invalidArg "o" "cannot compare values of wildly different types"
    static member (+) (a:distance, b:distance) =
        match a with
        | AU a -> AU (a + (distance.exAU b))
        | LY a -> LY (a + (distance.exLY b))
        | PC a -> PC (a + (distance.exPC b))
        | Km a -> Km (a + (distance.exKm b))
    static member (*) (a:distance, b:distance) =
        match a with
        | AU a -> AU (a * (distance.exAU b))
        | LY a -> LY (a * (distance.exLY b))
        | PC a -> PC (a * (distance.exPC b))
        | Km a -> Km (a * (distance.exKm b))
    static member (*) (a:decimal, b:distance) =
        match b with
        | AU x -> AU (a * x)
        | LY x -> LY (a * x)
        | PC x -> PC (a * x)
        | Km x -> Km (a * x)
    static member (*) (a:distance, b:decimal) = b*a
    /// Extract distance's raw value.
    static member extract (a:distance) =
        match a with
        | AU x -> x
        | LY x -> x
        | PC x -> x
        | Km x -> x
    /// Convert distance to LY.
    static member toLY (a:distance) =
        match a with
        | Km x -> Km x |> distance.toAU |> distance.toLY
        | AU x -> x / 63241.077088071m |> LY
        | PC x -> 3.2615637769m * x |> LY
        | _    -> a
    /// Convert distance to AU.
    static member toAU (a:distance) =
        match a with
        | Km x -> x / 149597870.691m |> AU
        | LY x -> x * 63241.077088071m |> AU
        | PC x -> PC x |> distance.toLY |> distance.toAU
        | _    -> a
    /// Convert distance to parsecs.
    static member toPC (a:distance) =
        match a with
        | Km x -> Km x |> distance.toAU |> distance.toLY |> distance.toPC
        | AU x -> AU x |> distance.toLY |> distance.toPC
        | LY x -> x / 3.2615637769m |> PC
        | _    -> a
    /// Convert distance to kilometers.
    static member toKm (a:distance) =
        match a with
        | AU x -> x * 149597870.691m |> Km
        | LY x -> LY x |> distance.toAU |> distance.toKm
        | PC x -> PC x |> distance.toLY |> distance.toAU
        | _    -> a
    //- Convert to T & then extract
    static member private exAU (a:distance) = distance.toAU a |> distance.extract
    static member private exLY (a:distance) = distance.toLY a |> distance.extract
    static member private exPC (a:distance) = distance.toPC a |> distance.extract
    static member private exKm (a:distance) = distance.toKm a |> distance.extract
    /// Format as float.
    member private m.fmtF =
        lazy ((match m with
               | AU a -> $"%0.3f{a}"
               | LY a -> $"%0.3f{a}"
               | PC a -> $"%0.3f{a}"
               | Km a -> $"%0.3f{a}").TrimEnd[|'0'|])
    /// Distance unit formatting...
    member private m.unitF =
        lazy (match m with
              | AU _ -> " AU"
              | LY _ -> " ly"
              | PC _ -> " pc"
              | Km _ -> "km")
    /// ToString(), yay!
    override m.ToString() = m.fmtF.Force() + m.unitF.Force()
    /// ToString() with/without unit fmt.
    member m.ToString(withUnit:bool) =
        match withUnit with
        | false -> m.fmtF.Force()
        | _     -> m.ToString()

let Km_Z = Km 0m
let AU_Z = AU 0m
let LY_Z = LY 0m
let PC_Z = PC 0m
let LZY0 = lazy (0)
