module StarSystem


type StarSystem =
    | S
    | D
    | T
    | Q
    //| Complex

let mkSys() =
    let s = Star.Star()
    printfn "%A" s
