namespace Day02Parser

module Warpping =
    let splitToInt (s:string) = s.Split 'x' |> Array.map int
    let extract a = (Array.head a, Array.item 1 a, Array.last a)

    let getDimension (str:string) =
        str.Split '\n' |> Array.map (splitToInt >> extract)
