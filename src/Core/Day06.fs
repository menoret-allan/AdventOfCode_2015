namespace Day06Core

module FireHazard =
    type Position = {X:int;Y:int}

    type Instruction = | TurnOn of (Position*Position) | Toogle of (Position*Position) | TurnOff of (Position*Position)
    type State = On | Off
    type Action = SwitchOn | SwitchOff | Switch

    type Board = Map<Position, State>

    let createBoard = seq {0..999} |> Seq.allPairs (seq {0..999}) |> Seq.map (fun (x,y) -> ({X=x;Y=y}, Off)) |> Map.ofSeq

    // let turnPositions state board {X=x1;Y=y1} {X=x2;Y=y2} =
    //     let test = seq {0..1} |> Seq.allPairs (seq {2..3}) |> Seq.map (fun (x,y) -> ({X=x;Y=y}))
    //     board

    let computeNewState s action =
        match (action, s) with
        | (SwitchOn, _) -> Some On
        | (SwitchOff, _) -> Some Off
        | (Switch, Some On) -> Some Off
        | (Switch, Some Off) -> Some On
        | _ -> failwith "unknown state inside the board"

    let turnPositions2 (action:Action) (board:Board) {X=x1;Y=y1} {X=x2;Y=y2} =
        seq {x1..x2} |> Seq.allPairs (seq {y1..y2})
            |> Seq.map (fun (x,y) -> ({X=x;Y=y}))
            |> Seq.fold (fun (b:Map<Position, State>) pos -> b.Change(pos, fun s -> computeNewState s action)) board

    let updateBoard board i =
        match i with
        | TurnOn (pos1, pos2) -> turnPositions2 SwitchOn board pos1 pos2
        | TurnOff (pos1, pos2) -> turnPositions2 SwitchOff board pos1 pos2
        | Toogle (pos1, pos2) -> turnPositions2 Switch board pos1 pos2


    let rec applyInst instructions board =
        match instructions with
        | [] -> board
        | i::rest -> applyInst rest (updateBoard board i)
    
    let howManyOn (instructions: Instruction list) =
        let board = createBoard
        applyInst instructions board |> Map.toSeq |> Seq.filter (fun (_, state) -> state = On) |> Seq.length

    let generatePos {X=x1;Y=y1} {X=x2;Y=y2} value =
        seq {x1..x2} |> Seq.allPairs (seq {y1..y2})
            |> Seq.map (fun (x,y) -> ({X=x;Y=y}, value))

    let rec generateBoard instructions =
        match instructions with
        | [] -> Seq.empty
        | (TurnOn (pos1, pos2))::rest -> seq {yield! generatePos pos1 pos2 1; yield! generateBoard rest }
        | (TurnOff (pos1, pos2))::rest -> seq {yield! generatePos pos1 pos2 -1; yield! generateBoard rest }
        | (Toogle (pos1, pos2))::rest -> seq {yield! generatePos pos1 pos2 2; yield! generateBoard rest }

    let calcList list =
        list |> Seq.fold (fun acc (_, n) -> if n = -1 && acc=0 then 0 else acc+n) 0

    let howManyOn2 (instructions: Instruction list) =
        let board = generateBoard instructions
        board |> Seq.groupBy (fun (pos, _) -> pos) |> Seq.sumBy (fun (_, list) -> calcList list)

