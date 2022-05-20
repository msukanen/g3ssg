open AstroMeasurement

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let au = AU 1m
let ly = distance.toLY au

printfn "%A AU = %A ly" au ly

StarSystem.mkSys()
