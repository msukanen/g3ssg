[<AutoOpen>]
module TimeMeasurement

[<CustomComparison; CustomEquality>]
type time =
    | EYears of decimal
    | Days of decimal
    | Hours of decimal
    | Mins of decimal
    | Secs of decimal
    /// Equality for all - or not... ke-ke-ke...
    override x.Equals o =
        match o with
        | :? time as y ->
            match x with
            | EYears z -> time.toE y |> time.extract = z
            | Days   z -> time.toD y |> time.extract = z
            | Hours  z -> time.toH y |> time.extract = z
            | Mins   z -> time.toM y |> time.extract = z
            | Secs   z -> time.toS y |> time.extract = z
        | _ -> false
    /// Hash is good, yes...
    override x.GetHashCode() =
        match x with
        | EYears z -> z.GetHashCode()
        | Days z   -> z.GetHashCode() + 1
        | Hours z  -> z.GetHashCode() + 2
        | Mins z   -> z.GetHashCode() + 3
        | Secs z   -> z.GetHashCode() + 4
    interface System.IComparable with
        /// Lets compare shit, ok?
        member x.CompareTo o =
            match o with
            | :? time as y ->
                match x with
                | EYears z -> compare z (time.toE y |> time.extract)
                | Days   z -> compare z (time.toD y |> time.extract)
                | Hours  z -> compare z (time.toH y |> time.extract)
                | Mins   z -> compare z (time.toM y |> time.extract)
                | Secs   z -> compare z (time.toS y |> time.extract)
            | _ -> invalidArg "o" "cannot compare values of wildly different types"
    /// <summary>
    /// Convert to Earth-Years.
    /// </summary>
    static member toE (v:time): time =
        match v with
        | Days x   -> x / 365.2422m |> EYears
        | Hours _  -> time.toD v |> time.toE
        | Mins _   -> time.toH v |> time.toE
        | Secs _   -> time.toM v |> time.toE
        | _        -> v
    /// <summary>
    /// Convert to Days.
    /// </summary>
    static member toD (diy:decimal, v:time): time =
        match v with
        | EYears x -> x * diy |> Days
        | Hours x  -> x/24m |> Days
        | Mins _   -> time.toD(diy, time.toH v)
        | Secs _   -> time.toD(diy, time.toH v)
        | _        -> v
    /// <summary>
    /// Convert to Days while assuming Earth-Year as year length.
    /// </summary>
    static member toD (v:time): time = time.toD (365.2422m, v)
    /// <summary>
    /// Convert to Hours.
    /// </summary>
    static member toH (v:time): time =
        match v with
        | EYears _ -> time.toD v |> time.toH
        | Days x   -> x*24m |> Hours
        | Mins x   -> x/60m |> Hours
        | Secs _   -> time.toM v |> time.toH
        | _        -> v
    /// <summary>
    /// Convert to Minutes.
    /// </summary>
    static member toM (v:time): time =
        match v with
        | EYears _ -> time.toH v |> time.toM
        | Days _   -> time.toH v |> time.toM
        | Hours x  -> x*60m |> Mins
        | Secs x   -> x/60m |> Mins
        | _        -> v
    /// <summary>
    /// Convert to Seconds.
    /// </summary>
    static member toS (v:time): time =
        match v with
        | EYears x -> x * 31_556_926.08m |> Secs
        | Days _   -> time.toM v |> time.toS
        | Hours _  -> time.toM v |> time.toS
        | Mins x   -> x*60m |> Secs
        | _        -> v
    static member (*) (a:time, b:decimal): time =
        match a with
        | EYears e -> e * b |> EYears
        | Days d   -> d * b |> Days
        | Hours h  -> h * b |> Hours
        | Mins m   -> m * b |> Mins
        | Secs s   -> s * b |> Secs
    static member (*) (a:decimal, b:time): time = b*a
    static member (*) (a:time, b:int):time = a * (decimal b)
    static member (*) (b:int, a:time):time = b*a
    static member (+) (a:time, b:time):time =
        match a with
        | EYears e -> e + (time.toE b |> time.extract) |> EYears
        | Days   d -> d + (time.toD b |> time.extract) |> Days
        | Hours  h -> h + (time.toH b |> time.extract) |> Hours
        | Mins   m -> m + (time.toM b |> time.extract) |> Mins
        | Secs   s -> s + (time.toS b |> time.extract) |> Secs
    /// <summary>
    /// Extract raw value.
    /// </summary>
    static member extract (a:time): decimal =
        match a with
        | EYears e -> e
        | Days   d -> d
        | Hours  h -> h
        | Mins   m -> m
        | Secs   s -> s
    static member Zero = Hours 0m
    static member EarthYear = Secs 31_556_926.08m
