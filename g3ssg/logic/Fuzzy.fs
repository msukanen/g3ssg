namespace Logic

module Fuzzy =
    let vp (p:int) (v:decimal): decimal =
        let d = decimal ((Dice.d ((abs p)*2+1) 1) - ((abs p)+1))
        let onep = 0.01m * v
        (onep * d) + v
