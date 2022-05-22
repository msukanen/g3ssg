module Dice

type System.Random with
    member this.GetValues(ma, mb) =
        Seq.initInfinite (fun _ -> this.Next(ma, mb))

let private rng = System.Random()

///<summary>
/// Roll some dice.
///</summary>
///<param name="s">Sides per die.</param>
///<param name="n">Number of dice to toss.</param>
let d s n: int =
    match s with
    | 0 -> 0
    | 1 -> n
    | _ -> Seq.sum (rng.GetValues(1, abs s) |> Seq.take (abs n))

///<summary>
/// Probability of 'v'.
///</summary>
///<param name="t">Threshold%</param>
///<param name="v">Value, if 't' is met.</param>
///<returns>'v' if 't' is met, otherwise 0 (zero).</returns>
let p t v =
    let x = d 100 1
    match x with
    | a when a <= t -> v
    | _ -> 0
