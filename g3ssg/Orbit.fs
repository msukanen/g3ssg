module Orbit
open Star

let baseD() = 0.1m * decimal(Dice.d 6 1)
let bodeC s =
    match s with
    | (Class.VI, Type.M, _, _, _, _, _, _) -> AU 0.2m
    | _ -> match Dice.d 3 1 with
           | 1 -> AU 0.3m
           | 2 -> AU 0.35m
           | _ -> AU 0.4m

let dist based bodec o =
    match o with
    | 1 -> based
    | 2 -> based + bodec
    | _ -> based + (decimal(2.**float(o-2)) * bodec)
