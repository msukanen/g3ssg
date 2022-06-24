module Element

open Star

/// <summary>
/// Moon types + number of them.
/// </summary>
type moontype =
    | Moonlet of int
    | Small of int
    | Medium of int
    | Large of int
    | Giant of int
    | SmallGG of int

/// <summary>
/// Interface for all sorts of orbit things.
/// </summary>
type IOrbit =
    abstract index : int     // orbit index
    abstract dist : distance // distance to mom star

/// <summary>
/// Interface for all sorts of planets.
/// </summary>
type IPlanet =
    abstract rad : distance
    abstract moons : moontype list
    abstract lenDay : time
    abstract axialTilt : decimal
    abstract lenYear : time  // length of year in Earth years/hours/etc


type OrbitalElement(s:Star, idx:int) =
    interface IOrbit with
        member _.dist = s.orbitDist idx
        member _.index = idx

/// <summary>
/// Represents an empty orbit.
/// </summary>
type EmptyOrbit(s:Star, idx:int) =
    inherit OrbitalElement(s, idx)
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> empty"

type Planet(s:Star, idx:int, rad:distance) =
    inherit OrbitalElement(s, idx)
    interface IPlanet with
        member _.lenYear =
            let d = distance.extract rad
            (d * d * d) / s.mass
                |> float   |> sqrt
                |> decimal |> EYears
        member _.rad = rad
        member _.moons = []
        member _.lenDay = time.Zero
        member _.axialTilt =
            match Dice.d 6 2 with
            | 2|3 -> 0m
            | a when a < 8 -> (Dice.d 6 1) * 3 |> decimal
            | a when a < 11 -> (Dice.d 6 2) + 20 |> decimal
            | 11 -> (Dice.d 6 3) + 30 |> decimal
            | _ -> let x = (Dice.d 6 1) * 10 + 40
                   if x > 90 then 90m
                   else decimal(x)


let private hasLGorBiggerMoon (ms:moontype list) =
    let mutable maybe = false
    for m in ms do
        match m with
        | Large _
        | Giant _
        | SmallGG _ -> maybe <- true
        | _ -> ()
    maybe

let private dlen idx rad hlm: time =
    let mutable d =
        match idx with
        | 1 -> -4
        | 2 -> -2
        | _ ->  0
    d <- d + match rad with
                | a when a < ER 0.5m -> -1
                | a when a > ER 9m -> 3
                | a when a > ER 6m -> 2
                | a when a > ER 3m -> 1
                | _ -> 0
    if hlm = true then
        d <- d - 1
    match (Dice.d 6 2) + d with
    | a when a < 3 -> (Dice.d 6 2) * 10 |> decimal |> Days
    | 3 -> (Dice.d 6 1) * 12 |> decimal |> Days
    | 4 -> (Dice.d 6 1) *  5 |> decimal |> Days
    | 5 -> (Dice.d 6 2) * 10 |> decimal |> Hours
    | 6 -> (Dice.d 6 1) * 10 |> decimal |> Hours
    | a when a > 10 -> Dice.d 6 3 |> decimal |> Hours
    | a -> Dice.d 6 (14-a) |> decimal |> Hours

type density =
    | GasGiant
    | Silicate
    | LowIron
    | MedIron
    | HiIron
    | Metallic
/// <summary>
/// Hold info about one or other terrestrial-type planet.
/// </summary>
type Terrestrial(s:Star, idx:int, rad:distance) =
    inherit Planet(s, idx, rad)
    let mns =
        let d = match rad with
                | a when a < ER 1m -> -1
                | a when a > ER 1.5m -> 1
                | _ -> 0
        let ml = (Dice.d 6 1) - 4 + d
        let sm = (Dice.d 6 1) - 4 + d
        let mm = (Dice.d 6 1) - 5 + d
        let lm = (Dice.d 6 1) - 5 + d
        let mutable mns = []
        if ml > 0 then
            mns <- moontype.Moonlet ml :: mns
        if sm > 0 then
            mns <- moontype.Small sm :: mns
        if mm > 0 then
            mns <- moontype.Medium mm :: mns
        if lm > 0 then
            mns <- moontype.Large lm :: mns
        mns
    new(s:Star, idx:int) = Terrestrial(s, idx, Mi (decimal(Dice.d 6 2)*500m) |> distance.toKm)
    interface IPlanet with
        override _.moons = mns
        override _.lenDay = dlen idx rad (hasLGorBiggerMoon mns)
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> terrestrial"

/// <summary>
/// Generic size categories of gas giants.
/// </summary>
type ggsize =
    | Small
    | Medium
    | Large
    | Huge
/// <summary>
/// Gas giant special features.
/// </summary>
type ggspecial =
    | RetrogradeMoon
    | InclinedOrbitMoon
    | FaintRing
    | SpectacularRing
    | AsteroidBelt
    | OortBelt
    | WarmsMoonToHabitable
/// <summary>
/// Contain gas giant related stuff.
/// </summary>
type GasGiant(s:Star, idx:int, rad:distance, sz:ggsize) =
    inherit Planet(s, idx, rad)
    // determine initial number of moons
    let mutable mns =
        let d = match sz with
                | Large -> 1
                | Huge  -> 2
                | _     -> 0
        let lm = (Dice.d 6 1) - 3 + d
        let gm = (Dice.d 6 1) - 5 + d
        let sg = (Dice.d 6 1) - 7 + d
        let mutable mns =
            [moontype.Moonlet ((Dice.d 6 3) + d)
            ;moontype.Small   ((Dice.d 6 2) + d)
            ;moontype.Medium  ((Dice.d 6 1) + 1 + d)]
        if lm > 0 then
            mns <- moontype.Large lm :: mns
        if gm > 0 then
            mns <- moontype.Giant gm :: mns
        if sg > 0 then
            mns <- moontype.SmallGG sg :: mns
        mns
    // determine special feature(s), if any
    let spes =
        let mutable ss = Set.empty
        let d = match sz with
                | Large -> 1
                | Huge  -> 2
                | _     -> 0
        let mutable n = 1
        while n > 0 do
            n <- n - 1
            match (Dice.d 6 3) + d with
            | 10 -> ss <- ss.Add(if Dice.d 6 1 < 4 then RetrogradeMoon else InclinedOrbitMoon)
            | 11|12|13 -> ss <- ss.Add(FaintRing)
            | 14 -> ss <- ss.Add(SpectacularRing)
            | 15 -> ss <- ss.Add(AsteroidBelt)
            | 16 -> ss <- ss.Add(OortBelt)
            | 17 -> mns <- mns |> List.map (fun m ->
                           match m with
                           | Moonlet n -> Moonlet (n*2)
                           | moontype.Small n -> moontype.Small (n*2)
                           | moontype.Medium n -> moontype.Medium (n*2)
                           | moontype.Large n -> moontype.Large (n*2)
                           | moontype.Giant n -> moontype.Giant (n*2)
                           | moontype.SmallGG n -> moontype.SmallGG (n*2))
            | 18 -> n <- n + 2
            | a when a > 18 -> ss <- ss.Add(WarmsMoonToHabitable)
            | _ -> ()
        ss

    /// <summary>
    /// CTOR gas giant w/o predefined radius.
    /// </summary>
    private new(s:Star, idx:int, sz:ggsize) =
        GasGiant(s, idx,
                 (match sz with
                  | Small  -> Mi 15_000m |> distance.toKm
                  | Medium -> Mi 25_000m |> distance.toKm
                  | Large  -> Mi 40_000m |> distance.toKm
                  | _      -> Mi 100_000m |> distance.toKm),
                 sz)
    /// <summary>
    /// CTOR a completely randomized gas giant.
    /// </summary>
    new(s:Star, idx:int) =
        let m = match s.color with
                | Class.M -> -2
                | Class.K -> -1
                | _      ->  0
        GasGiant(s, idx,
                 match (Dice.d 6 3) + m with
                 | 3 -> Huge
                 | 4 -> Huge
                 | a when a < 9 -> Small
                 | a when a < 13 -> Medium
                 | _ -> Large)

    interface IPlanet with
        override _.moons = mns
        override _.lenDay = dlen idx rad (hasLGorBiggerMoon mns)
    
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> gas giant"
    
    member _.specialFeatures = spes

/// <summary>
/// Base asteroid types.
/// </summary>
type astbasetype = M | S | C
/// <summary>
/// Effective asteroid types.
/// </summary>
type asttype = Asteroid of astbasetype | IcyAsteroid of astbasetype
/// <summary>
/// Asteroid belt container.
/// </summary>
type AsteroidBelt(s:Star, idx:int, t:asttype) =
    inherit OrbitalElement(s, idx)
    new(s:Star, idx:int) =
        let icy = Dice.d 6 3 > 17
        let rec ch() =
            match Dice.d 6 3 with
            | 3|4 -> M
            | 18  -> ch()
            | a when a < 14 -> S
            | _   -> C
        AsteroidBelt(s, idx,
                     if icy = true then IcyAsteroid (ch())
                     else Asteroid (ch()))
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> asteroid belt"
    member m.basetype =
        match m.fulltype with
        | Asteroid x -> x                
        | IcyAsteroid x -> x
    member m.icy =
        match m.fulltype with
        | IcyAsteroid _ -> true
        | _ -> false
    member _.fulltype = t

/// <summary>
/// Generate orbit elements for the given <see cref="Star"/>.
/// </summary>
/// <returns>A list of orbit elements (or an empty list if none exist).</returns>
let mkElements (s:Star): IOrbit list =
    // closer than biozone?
    let bzC i r : IOrbit =
        match r with
        | 2|3|4 -> EmptyOrbit (s,i)
        | 5|6   -> Terrestrial (s,i)
        | 7|8|9 -> Terrestrial (s,i)
        | 10|11 -> AsteroidBelt (s,i)
        | _     -> GasGiant (s,i)
    // within goldilocks?
    let bzG i r : IOrbit =
        match r with
        | 2|3 -> EmptyOrbit (s,i)
        | a when a < 9 -> Terrestrial (s,i)
        | 9|10 -> AsteroidBelt (s,i)
        | 11 -> GasGiant (s,i)
        | _  -> GasGiant (s,i)
    // beyond biozone?
    let bzF i r : IOrbit =
        match r with
        | 1 -> Terrestrial (s,i)
        | 2 -> AsteroidBelt (s,i)
        | 3 -> EmptyOrbit (s,i)
        | 7 -> Terrestrial (s,i)
        | _ -> GasGiant (s,i)
    if s.numOrbits > 0 then
        let mutable os: IOrbit list = []
        for i in 1 .. s.numOrbits do
            let mutable e =
                lazy (match s.orbitBz i with
                      | TooClose -> bzC i (Dice.d 6 2)
                      | Goldilocks -> bzG i (Dice.d 6 2)
                      | TooFar -> bzF i (Dice.d 6 1)
                      | TooFar10 -> bzF i ((Dice.d 6 1) + 1))
            if s.orbitDist i <= s.innerLimit then
                os <- EmptyOrbit (s,i)::os
            elif s.size = Type.D then
                // <1 AU all vaporize, <40 all planets lose atmo, <80 ggs turn to terrestrials
                if s.orbitDist i <= AU 1m then
                    os <- EmptyOrbit (s,i)::os
                elif s.orbitDist i <= AU 40m then
                    match e.Force() with
                    | :? GasGiant as a -> os <- (Terrestrial (s,i))::os
                    | :? Terrestrial as a -> os <- (Terrestrial (s,i))::os
                    | x -> os <- x::os
                elif s.orbitDist i <= AU 80m then
                    match e.Force() with
                    | :? GasGiant as a -> os <- (Terrestrial (s,i))::os
                    | x -> os <- x::os
                else os <- e.Force()::os
            else os <- e.Force()::os
        // eradicate all EmptyOrbit instances
        os |> List.filter (fun x -> match x with | :? EmptyOrbit as _ -> false | _ -> true)
    else []
