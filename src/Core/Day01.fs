namespace Day01Core

module Building =
    let floorCount c = if c = '(' then 1 else -1

    let floorCounter (str:string) =
        str |> Seq.sumBy floorCount

    let basementPosition (str:string) =
        let rec matchBasement inst count pos =
            match (count, inst) with
            | (-1, _) -> pos
            | (_, c::rest) -> matchBasement rest (count + floorCount c) (pos+1)
            | _ -> failwith "Never reach the basement"
        matchBasement (str |> Seq.toList) 0 0