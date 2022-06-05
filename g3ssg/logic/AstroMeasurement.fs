[<AutoOpen>]
module rec AstroMeasurement

[<CustomComparison; CustomEquality>]
type distance =
    | AU of decimal
    | LY of decimal
    | PC of decimal
    | Km of decimal
    | ER of decimal // Earth Radius
    | Mi of decimal
    /// Equality for all!
    override x.Equals o =
        match o with
        | :? distance as y ->
            match x with
            | AU z -> z = distance.exAU y
            | LY z -> z = distance.exLY y
            | PC z -> z = distance.exPC y
            | Km z -> z = distance.exKm y
            | ER z -> z = distance.exER y
            | Mi z -> z = distance.exMi y
        | _ -> false
    /// Hash is good, yes...
    override x.GetHashCode() =
        match x with
        | AU z -> z.GetHashCode()
        | LY z -> z.GetHashCode() + 1
        | PC z -> z.GetHashCode() + 2
        | Km z -> z.GetHashCode() + 3
        | ER z -> z.GetHashCode() + 4
        | Mi z -> z.GetHashCode() + 5
    interface System.IComparable with
        /// Lets compare shit, ok?
        member x.CompareTo o =
            match o with
            | :? distance as y ->
                match x with
                | AU z -> compare z (distance.exAU y)
                | LY z -> compare z (distance.exLY y)
                | PC z -> compare z (distance.exPC y)
                | Km z -> compare z (distance.exKm y)
                | ER z -> compare z (distance.exER y)
                | Mi z -> compare z (distance.exMi y)
            | _ -> invalidArg "o" "cannot compare values of wildly different types"
    /// 1+1 = ?
    static member (+) (a:distance, b:distance) =
        match a with
        | AU a -> AU (a + (distance.exAU b))
        | LY a -> LY (a + (distance.exLY b))
        | PC a -> PC (a + (distance.exPC b))
        | Km a -> Km (a + (distance.exKm b))
        | ER a -> ER (a + (distance.exER b))
        | Mi a -> Mi (a + (distance.exMi b))
    /// AU * LY = ?
    static member (*) (a:distance, b:distance) =
        match a with
        | AU a -> AU (a * (distance.exAU b))
        | LY a -> LY (a * (distance.exLY b))
        | PC a -> PC (a * (distance.exPC b))
        | Km a -> Km (a * (distance.exKm b))
        | ER a -> ER (a * (distance.exER b))
        | Mi a -> Mi (a * (distance.exMi b))
    /// 1.2m * AU = ?
    static member (*) (a:decimal, b:distance) =
        match b with
        | AU x -> AU (a * x)
        | LY x -> LY (a * x)
        | PC x -> PC (a * x)
        | Km x -> Km (a * x)
        | ER x -> ER (a * x)
        | Mi x -> Mi (a * x)
    /// AU * 1.2m = ?
    static member (*) (a:distance, b:decimal) = b*a
    /// Extract distance's raw value.
    static member extract (a:distance) =
        match a with
        | AU x -> x
        | LY x -> x
        | PC x -> x
        | Km x -> x
        | ER x -> x
        | Mi x -> x
    /// Convert distance to LY.
    static member toLY (a:distance) =
        match a with
        | Km x -> Km x |> distance.toAU |> distance.toLY
        | AU x -> x / 63_241.077088071m |> LY
        | PC x -> 3.2615637769m * x |> LY
        | ER x -> ER x |> distance.toKm |> distance.toLY
        | Mi x -> Mi x |> distance.toKm |> distance.toLY
        | _    -> a
    /// Convert distance to AU.
    static member toAU (a:distance) =
        match a with
        | Km x -> x / 149_597_870.691m |> AU
        | LY x -> x * 63_241.077088071m |> AU
        | PC x -> PC x |> distance.toLY |> distance.toAU
        | ER x -> ER x |> distance.toKm |> distance.toAU
        | Mi x -> Mi x |> distance.toKm |> distance.toAU
        | _    -> a
    /// Convert distance to parsecs.
    static member toPC (a:distance) =
        match a with
        | Km x -> Km x |> distance.toAU |> distance.toLY |> distance.toPC
        | ER x -> ER x |> distance.toKm |> distance.toPC
        | AU x -> AU x |> distance.toLY |> distance.toPC
        | LY x -> x / 3.2615637769m |> PC
        | Mi x -> Mi x |> distance.toKm |> distance.toPC
        | _    -> a
    /// Convert distance to kilometers.
    static member toKm (a:distance) =
        match a with
        | AU x -> x * 149_597_870.691m |> Km
        | LY x -> LY x |> distance.toAU |> distance.toKm
        | PC x -> PC x |> distance.toLY |> distance.toAU
        | ER x -> Km 6_371m
        | Mi x -> Km 1.609344m
        | _    -> a
    static member toER (a:distance) =
        match a with
        | AU x -> AU x |> distance.toKm |> distance.toER
        | LY x -> LY x |> distance.toKm |> distance.toER
        | PC x -> PC x |> distance.toKm |> distance.toER
        | Km x -> x / 6371m |> ER
        | Mi x -> Mi x |> distance.toKm |> distance.toER
        | _    -> a
    static member toMi (a:distance) =
        match a with
        | AU x -> AU x |> distance.toKm |> distance.toMi
        | LY x -> LY x |> distance.toKm |> distance.toMi
        | PC x -> PC x |> distance.toKm |> distance.toMi
        | Km x -> x / 1.609344m |> Mi
        | ER x -> ER x |> distance.toKm |> distance.toMi
        | _    -> a
    //- Convert to T & then extract
    static member private exAU (a:distance) = distance.toAU a |> distance.extract
    static member private exLY (a:distance) = distance.toLY a |> distance.extract
    static member private exPC (a:distance) = distance.toPC a |> distance.extract
    static member private exKm (a:distance) = distance.toKm a |> distance.extract
    static member private exER (a:distance) = distance.toER a |> distance.extract
    static member private exMi (a:distance) = distance.toMi a |> distance.extract
    /// Format as float.
    member private m.fmtF =
        lazy ((match m with
               | AU a -> $"%0.3f{a}"
               | LY a -> $"%0.3f{a}"
               | PC a -> $"%0.3f{a}"
               | Km a -> $"%0.3f{a}"
               | ER a -> $"%0.3f{a}"
               | Mi a -> $"%0.3f{a}"
               ).TrimEnd[|'0'|])
    /// Distance unit formatting...
    member private m.unitF =
        lazy (match m with
              | AU _ -> " AU"
              | LY _ -> " ly"
              | PC _ -> " pc"
              | Km _ -> "km"
              | ER _ -> " E.D."
              | Mi _ -> "mi"
              )
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
