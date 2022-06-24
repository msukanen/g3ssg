module rec Star
open AstroMeasurement

/// <summary>
/// Star classes/color.
/// </summary>
type Class = O | B | A | F | G | K | M | D
/// <summary>
/// Star types/generic sizery.
/// </summary>
type Type =
    | D
    | VI
    | V
    | IV
    | III
    | II
    | Ia
    | Ib
/// <summary>
/// Biozone designators.
/// </summary>
type Biozone =
    | TooClose
    | Goldilocks
    | TooFar   | TooFar10
/// <summary>
/// Make star class.
/// </summary>
let rec mkClass (a:Type): Class =
    match a with
    | VI -> match Dice.d 6 1 with
            | 1 -> G
            | 2 -> K
            | _ -> M
    | V -> match Dice.d 6 3 with
            | 3 -> O
            | 4 -> B
            | 5 -> A
            | 6 -> F
            | 7 -> G
            | 8 -> K
            | _ -> M
    | D -> Class.D
    | _ -> match Dice.d 6 2 with
            | 2 -> match a with
                   | II
                   | III
                   | IV -> mkClass a
                   | _ -> O
            | 3 -> match a with
                   | IV -> mkClass a
                   | _ -> M
            | 4
            | 5 -> B
            | x when x < 10 -> K
            | _ -> A
/// <summary>
/// Make star type and class.
/// </summary>
/// <param name="all">Accept all types as result?</param>
let rec mkTypeClass (all:bool): Type * Class =
    match Dice.d 6 3 with
    | a when a < 6 && all = false -> mkTypeClass(all)
    | a when a < 6 -> (Type.D, Class.D)
    | 6 -> (VI, mkClass VI)
    | a when a < 18 -> (V, mkClass V)
    | _ -> match Dice.d 6 3 with
           | 3 -> match Dice.d 3 1 with
                  | 1 -> (Ia, mkClass Ia)
                  | _ -> (Ib, mkClass Ib)
           | 4 -> (II, mkClass II)
           | a when a < 13 -> (III, mkClass III)
           | _ -> (IV, mkClass IV)

/// <summary>
/// Make a star's data chunk.
/// </summary>
/// <param name="all">Accept all kinds of stars as result?</param>
let rec mkStar'(all:bool) =
    let bodeC (t:Type) (c:Class): distance =
        match (t,c) with
        | (Type.VI, Class.M) -> AU 0.2m
        | _ -> match Dice.d 3 1 with
               | 1 -> AU 0.3m
               | 2 -> AU 0.35m
               | _ -> AU 0.4m
    let mutable bC: distance option = None
    let bD = 0.1m * decimal(Dice.d 6 1) |> AU
    let (t, c) = mkTypeClass(all)
    let (m, z, i, r, p, n, l) =
        match c with
        | O -> match t with
               | Ia -> (70m, AU 790m, AU 16m, AU 0.2m, 0, LZY0, -12)
               | Ib -> (60m, AU 630m, AU 13m, AU 0.1m, 0, LZY0, -12)
               | V  -> (50m, AU 500m, AU 10m, AU_Z, 0, LZY0, -9)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | B -> match t with
               | Ia -> (50m, AU 500m, AU 10m, AU 0.2m, 0, LZY0, -10)
               | Ib -> (40m, AU 320m, AU 6.3m, AU 0.1m, 0, LZY0, -10)
               | II -> (35m, AU 250m, AU 5.m, AU 0.1m, 3, lazy ((Dice.d 6 3)+1), -10)
               | III-> (30m, AU 200m, AU 4.m, AU_Z, 3, lazy ((Dice.d 6 3)+1), -10)
               | IV -> (20m, AU 180m, AU 3.8m, AU_Z, 3, lazy ((Dice.d 6 3)+1), -10)
               | V  -> (10m, AU 30m, AU 0.6m, AU_Z, 4, lazy (Dice.d 6 3), -9)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | A -> match t with
               | Ia -> (30m, AU 200m, AU 4m, AU 0.6m, 3, lazy ((Dice.d 6 3)+3), -10)
               | Ib -> (16m, AU 50m, AU 1m, AU 0.2m, 3, lazy ((Dice.d 6 3)+2), -10)
               | II -> (10m, AU 20m, AU 0.4m, AU_Z, 3, lazy ((Dice.d 6 3)+2), -10)
               | III-> (6m, AU 5m, AU_Z, AU_Z, 3, lazy ((Dice.d 6 3)+1), -10)
               | IV -> (4m, AU 4m, AU_Z, AU_Z, 4, lazy (Dice.d 6 3), -10)
               | V  -> (3m, AU 3.1m, AU_Z, AU_Z, 5, lazy ((Dice.d 6 3)-1), -9)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | F -> match t with
               | Ia -> (15m, AU 200m, AU 4m, AU 0.8m, 4, lazy ((Dice.d 6 3)+3), -10)
               | Ib -> (13m, AU 50m, AU 1m, AU 0.2m, 4, lazy ((Dice.d 6 3)+2), -10)
               | II -> (8m, AU 13m, AU 0.3m, AU_Z, 4, lazy ((Dice.d 6 3)+1), -9)
               | III-> (2.5m, AU 2.5m, AU 0.1m, AU_Z, 4, lazy (Dice.d 6 3), -9)
               | IV -> (2.2m, AU 2m, AU_Z, AU_Z, 6, lazy (Dice.d 6 3), -9)
               | V  -> (1.9m, AU 1.6m, AU_Z, AU_Z, 13, lazy ((Dice.d 6 3)-1), -8)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | G -> match t with
               | Ia -> (12m, AU 160m, AU 3.1m, AU 1.4m, 6, lazy ((Dice.d 6 3)+3), -10)
               | Ib -> (10m, AU 50m, AU 1m, AU 0.4m, 6, lazy ((Dice.d 6 3)+2), -10)
               | II -> (6m, AU 13m, AU 0.3m, AU 0.1m, 6, lazy ((Dice.d 6 3)+1), -9)
               | III-> (2.7m, AU 3.1m, AU 0.1m, AU_Z, 6, lazy (Dice.d 6 3), -8)
               | IV -> (1.8m, AU 1m, AU_Z, AU_Z, 7, lazy ((Dice.d 6 3)-1), -6)
               | V  -> (1.1m, AU 0.8m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-2), 0)
               | VI -> (0.8m, AU 0.5m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 2)+1), 1)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | K -> match t with
               | Ia -> (15m, AU 125m, AU 2.5m, AU 3m, 10, lazy ((Dice.d 6 3)+2), -10)
               | Ib -> (12m, AU 50m, AU 1m, AU 1m, 16, lazy ((Dice.d 6 3)+2), -10)
               | II -> (6m, AU 13m, AU 0.3m, AU 0.2m, 16, lazy ((Dice.d 6 3)+1), -9)
               | III-> (3m, AU 4m, AU 0.1m, AU_Z, 16, lazy (Dice.d 6 3), -7)
               | IV -> (2.3m, AU 1m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-1), -5)
               | V  -> (0.9m, AU 0.4375m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-2), 0)
               | VI -> (0.5m, AU 0.2m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 2)+1), 1)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | M -> match t with
               | Ia -> (20m, AU 100m, AU 2m, AU 7m, 16, lazy (Dice.d 6 3), -10)
               | Ib -> (16m, AU 50m, AU 1m, AU 4.2m, 16, lazy (Dice.d 6 3), -10)
               | II -> (8m, AU 16m, AU 0.3m, AU 1.1m, 16, lazy (Dice.d 6 3), -9)
               | III-> (4m, AU 5m, AU 0.1m, AU 0.3m, 16, lazy (Dice.d 6 3), -6)
               | V  -> (0.3m, AU 0.1m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-2), 1)
               | VI -> (0.2m, AU 0.08m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 2)+2), 2)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | Class.D -> let (dc,dt,_,_,_,_,dp,dn,_,_,_) = mkStar'(false)
                     bC <- bodeC dc dt |> Some
                     (0.8m, AU 0.027m, AU_Z, AU_Z, dp, dn, -10)
    if bC = None then
       bC <- bodeC t c |> Some
    (t, c, m, z, i, r, p, n, l, bC.Value, bD)

/// <summary>
/// Make a star, or rather, data chunk to be fed to Star's constructor.
/// </summary>
/// <param name="all">Accept all sorts of stars as result?</param>
let rec mkStar(all:bool) =
    let (t, c, m, z, i, r, p, n, l, bc, bd) = mkStar'(all)
    let hasp = p > 0 && Dice.d 6 3 <= p
    (t, c, m, z, i, r, (if hasp then n.Force() else 0), l, bc, bd)

/// <summary>
/// Contain star related info.
/// </summary>
type Star(t:Type,c:Class,m,z:distance,i,r,p,l, bc, bd) =
    private new((t,c,m,z,i,r,p,l,bc,bd)) = Star(t,c,m,z,i,r,p,l,bc,bd)
    new() = Star(mkStar(true))
    member _.size = t
    member _.color = c // 'class', but as class is a reserved word in F# ...
    member _.mass = Logic.Fuzzy.vp 5 m
    member _.biozone = (z, 1.5m * z)
    member _.innerLimit = i
    member _.radius = r
    member _.numOrbits = p
    member _.LRM = l
    member _.luminosity = (Dice.d 10 1) - 1
    member _.orbitDist o : distance =
        match o with
        | 1 -> bd
        | 2 -> bd + bc
        | _ -> bd + (decimal(2.**float(o-2)) * bc)
    member m.orbitBz o =
        match o with
        | a when m.orbitClose a
            -> TooClose
        | a when m.orbitGoldilocks a
            -> Goldilocks
        | a when m.orbitDist a > 10m * (snd m.biozone)
            -> TooFar10
        | _ -> TooFar
    member m.orbitClose o = m.orbitDist o < fst m.biozone
    member m.orbitOut o   = m.orbitDist o > snd m.biozone
    member m.orbitGoldilocks o = not (m.orbitClose o || m.orbitOut o)
    override s.ToString() =
        $"%A{s.color}%i{s.luminosity} %A{s.size} - SM:%0.3f{s.mass} BZ:{(fst s.biozone).ToString(false)}-{(snd s.biozone)}"
