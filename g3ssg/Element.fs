module Element

open Star

type IOrbit =
    abstract index : int

type OrbitalElement(idx:int) =
    interface IOrbit with
        member m.index = idx

type EmptyOrbit(idx:int) =
    inherit OrbitalElement(idx)
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> empty"

type Terrestrial(idx:int) =
    inherit OrbitalElement(idx)
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> terrestrial"

type GasGiant(idx:int) = 
    inherit OrbitalElement(idx)
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> gas giant"

type AsteroidBelt(idx:int) =
    inherit OrbitalElement(idx)
    override m.ToString() =
        $"orbit #{(m:>IOrbit).index} -> asteroid belt"

let mkElements(s:Star): IOrbit list =
    let mutable os: IOrbit list = []
    let bzi i r : IOrbit =
        match r with
        | 2|3|4 -> EmptyOrbit i
        | 5|6   -> Terrestrial i
        | 7|8|9 -> Terrestrial i
        | 10|11 -> AsteroidBelt i
        | _     -> GasGiant i
    let bzg i r : IOrbit =
        match r with
        | 2 | 3 -> EmptyOrbit i
        | a when a < 9 -> Terrestrial i
        | 9 | 10 -> AsteroidBelt i
        | 11 -> GasGiant i
        | _  -> GasGiant i
    let bzf i r : IOrbit =
        match r with
        | 1 -> Terrestrial i
        | 2 -> AsteroidBelt i
        | 3 -> EmptyOrbit i
        | 7 -> Terrestrial i
        | _ -> GasGiant i
    if s.numOrbits > 0 then
        for i in 1 .. s.numOrbits do
            let mutable e =
                match s.orbitBz i with
                    | TooClose -> bzi i (Dice.d 6 2)
                    | Goldilocks -> bzg i (Dice.d 6 2)
                    | TooFar -> bzf i (Dice.d 6 1)
                    | TooFar10 -> bzf i ((Dice.d 6 1) + 1)
            if s.size = Class.D then
                if (s.orbitDist i) <= (AU 1m) then
                    ()
    os
