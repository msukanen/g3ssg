module rec Star
open AstroMeasurement

type Type = O | B | A | F | G | K | M | D
type Class =
    | D
    | VI
    | V
    | IV
    | III
    | II
    | Ia
    | Ib

let rec mkType (a:Class): Type =
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
    | D -> Type.D
    | _ -> match Dice.d 6 2 with
            | 2 -> match a with
                   | II
                   | III
                   | IV -> mkType a
                   | _ -> O
            | 3 -> match a with
                   | IV -> mkType a
                   | _ -> M
            | 4
            | 5 -> B
            | x when x < 10 -> K
            | _ -> A

let mkClass() =
    match Dice.d 6 3 with
    | a when a < 6 -> (Class.D, Type.D)
    | 6 -> (VI, mkType VI)
    | a when a < 18 -> (V, mkType V)
    | _ -> match Dice.d 6 3 with
           | 3 -> match Dice.d 3 1 with
                  | 1 -> (Ia, mkType Ia)
                  | _ -> (Ib, mkType Ib)
           | 4 -> (II, mkType II)
           | a when a < 13 -> (III, mkType III)
           | _ -> (IV, mkType IV)

let rec mkStar'() =
    let (c, t) = mkClass()
    let (m, z, i, r, p, n, l) =
        match t with
        | O -> match c with
               | Ia -> (70m, AU 790m, AU 16m, AU 0.2m, 0, LZY0, -12)
               | Ib -> (60m, AU 630m, AU 13m, AU 0.1m, 0, LZY0, -12)
               | V  -> (50m, AU 500m, AU 10m, AU_Z, 0, LZY0, -9)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | B -> match c with
               | Ia -> (50m, AU 500m, AU 10m, AU 0.2m, 0, LZY0, -10)
               | Ib -> (40m, AU 320m, AU 6.3m, AU 0.1m, 0, LZY0, -10)
               | II -> (35m, AU 250m, AU 5.m, AU 0.1m, 3, lazy ((Dice.d 6 3)+1), -10)
               | III-> (30m, AU 200m, AU 4.m, AU_Z, 3, lazy ((Dice.d 6 3)+1), -10)
               | IV -> (20m, AU 180m, AU 3.8m, AU_Z, 3, lazy ((Dice.d 6 3)+1), -10)
               | V  -> (10m, AU 30m, AU 0.6m, AU_Z, 4, lazy (Dice.d 6 3), -9)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | A -> match c with
               | Ia -> (30m, AU 200m, AU 4m, AU 0.6m, 3, lazy ((Dice.d 6 3)+3), -10)
               | Ib -> (16m, AU 50m, AU 1m, AU 0.2m, 3, lazy ((Dice.d 6 3)+2), -10)
               | II -> (10m, AU 20m, AU 0.4m, AU_Z, 3, lazy ((Dice.d 6 3)+2), -10)
               | III-> (6m, AU 5m, AU_Z, AU_Z, 3, lazy ((Dice.d 6 3)+1), -10)
               | IV -> (4m, AU 4m, AU_Z, AU_Z, 4, lazy (Dice.d 6 3), -10)
               | V  -> (3m, AU 3.1m, AU_Z, AU_Z, 5, lazy ((Dice.d 6 3)-1), -9)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | F -> match c with
               | Ia -> (15m, AU 200m, AU 4m, AU 0.8m, 4, lazy ((Dice.d 6 3)+3), -10)
               | Ib -> (13m, AU 50m, AU 1m, AU 0.2m, 4, lazy ((Dice.d 6 3)+2), -10)
               | II -> (8m, AU 13m, AU 0.3m, AU_Z, 4, lazy ((Dice.d 6 3)+1), -9)
               | III-> (2.5m, AU 2.5m, AU 0.1m, AU_Z, 4, lazy (Dice.d 6 3), -9)
               | IV -> (2.2m, AU 2m, AU_Z, AU_Z, 6, lazy (Dice.d 6 3), -9)
               | V  -> (1.9m, AU 1.6m, AU_Z, AU_Z, 13, lazy ((Dice.d 6 3)-1), -8)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | G -> match c with
               | Ia -> (12m, AU 160m, AU 3.1m, AU 1.4m, 6, lazy ((Dice.d 6 3)+3), -10)
               | Ib -> (10m, AU 50m, AU 1m, AU 0.4m, 6, lazy ((Dice.d 6 3)+2), -10)
               | II -> (6m, AU 13m, AU 0.3m, AU 0.1m, 6, lazy ((Dice.d 6 3)+1), -9)
               | III-> (2.7m, AU 3.1m, AU 0.1m, AU_Z, 6, lazy (Dice.d 6 3), -8)
               | IV -> (1.8m, AU 1m, AU_Z, AU_Z, 7, lazy ((Dice.d 6 3)-1), -6)
               | V  -> (1.1m, AU 0.8m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-2), 0)
               | VI -> (0.8m, AU 0.5m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 2)+1), 1)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | K -> match c with
               | Ia -> (15m, AU 125m, AU 2.5m, AU 3m, 10, lazy ((Dice.d 6 3)+2), -10)
               | Ib -> (12m, AU 50m, AU 1m, AU 1m, 16, lazy ((Dice.d 6 3)+2), -10)
               | II -> (6m, AU 13m, AU 0.3m, AU 0.2m, 16, lazy ((Dice.d 6 3)+1), -9)
               | III-> (3m, AU 4m, AU 0.1m, AU_Z, 16, lazy (Dice.d 6 3), -7)
               | IV -> (2.3m, AU 1m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-1), -5)
               | V  -> (0.9m, AU 0.4375m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-2), 0)
               | VI -> (0.5m, AU 0.2m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 2)+1), 1)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | M -> match c with
               | Ia -> (20m, AU 100m, AU 2m, AU 7m, 16, lazy (Dice.d 6 3), -10)
               | Ib -> (16m, AU 50m, AU 1m, AU 4.2m, 16, lazy (Dice.d 6 3), -10)
               | II -> (8m, AU 16m, AU 0.3m, AU 1.1m, 16, lazy (Dice.d 6 3), -9)
               | III-> (4m, AU 5m, AU 0.1m, AU 0.3m, 16, lazy (Dice.d 6 3), -6)
               | V  -> (0.3m, AU 0.1m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 3)-2), 1)
               | VI -> (0.2m, AU 0.08m, AU_Z, AU_Z, 16, lazy ((Dice.d 6 2)+2), 2)
               | _  -> raise (new System.ArgumentOutOfRangeException("Oops!"))
        | Type.D -> let (_,_,_,_,_,_,dp,dn,_) = mkStar'()
                    (0.8m, AU 0.027m, AU_Z, AU_Z, dp, dn, -10)
    (c, t, m, z, i, r, p, n, l)

let rec mkStar() =
    let (c, t, m, z, i, r, p, n, l) = mkStar'()
    let hasp = p > 0 && Dice.d 6 3 <= p
    (c, t, m, z, i, r, (if hasp then n.Force() else 0), l)

type Star(c,t,m,z:distance,i,r,p,l) =
    private new((c,t,m,z,i,r,p,l)) = Star(c,t,m,z,i,r,p,l)
    new() = Star(mkStar())
    member _.size = c
    member _.color = t
    member _.mass = m
    member _.biozone = (z, 1.5m * z)
    member _.innerLimit = i
    member _.radius = r
    member _.numOrbits = p
    member _.LRM = l
    member _.luminosity = (Dice.d 10 1) - 1
    override s.ToString() =
        $"%A{s.color}%i{s.luminosity} %A{s.size} - SM:%0.1f{s.mass} BZ:{(fst s.biozone).ToString(false)}-{(snd s.biozone)}"
