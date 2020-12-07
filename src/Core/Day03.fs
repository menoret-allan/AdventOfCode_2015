namespace Day03Core

module Movement =
    type Pos = {X:int;Y:int}

    let rec compute (x,y) moves =
        match moves with
        | [] -> seq {yield {X=0;Y=0}}
        | '>'::rest -> seq {yield {X=x+1;Y=y};yield! compute (x+1, y) rest}
        | '<'::rest -> seq {yield {X=x-1;Y=y};yield! compute (x-1, y) rest}
        | '^'::rest -> seq {yield {X=x;Y=y+1};yield! compute (x, y+1) rest}
        | 'v'::rest -> seq {yield {X=x;Y=y-1};yield! compute (x, y-1) rest}
        | _ -> failwith  "Unexpected char"


    let countHouses (str:string) =
        compute (0,0) (str |> Seq.toList) |> Seq.distinct |> Seq.length