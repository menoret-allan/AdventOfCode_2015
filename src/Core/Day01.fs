namespace Core

module Building =
    let floorCounter (str:string) =
        str |> Seq.sumBy (fun c -> if c = '(' then 1 else -1)
