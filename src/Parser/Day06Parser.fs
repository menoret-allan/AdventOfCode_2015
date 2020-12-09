namespace Day06Parser

open Day06Core.FireHazard
open Utils

module FireHazardParser =
    let (|Range|) (str:string) =
        let r = str.Split " through "
        let r1 = (Array.head r).Split ',' 
        let r2 = (Array.last r).Split ','
        ({X=Array.head r1 |> int; Y=Array.last r1 |> int}, {X=Array.head r2 |> int; Y=Array.last r2 |> int})

    let extractRange range = match range with Range(pos) -> pos

    let translate str =
        match str with
        | Prefix "turn on " rest -> TurnOn (extractRange rest)
        | Prefix "toggle " rest -> Toogle (extractRange rest)
        | Prefix "turn off " rest -> TurnOff (extractRange rest)
        | _ -> failwith ("Unknown instruction: " + str)

    let scan (str:string) =
        str.Split '\n' |> Array.toList |> List.map translate
