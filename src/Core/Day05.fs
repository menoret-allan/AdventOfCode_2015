namespace Day05Core

module Nice =

    let isIt (str:string) =
        let has3Volwel = str |> Seq.filter (fun c -> Seq.contains c "aeiou") |> Seq.length >= 3
        let paired = str |> Seq.pairwise
        let hasRepeatedLetter = paired |> Seq.exists (fun (x,y) -> x=y)
        let doesNotContainEnemy = paired |> set |> Set.intersect (set [('a','b');('c','d');('p','q');('x','y');]) |> Set.isEmpty
        has3Volwel && hasRepeatedLetter && doesNotContainEnemy

    let howMany (strs:string) =
        strs.Split '\n' |> Array.filter isIt |> Array.length

    let verifyMatch (_, s) =
        match (Seq.length s, Seq.head s, Seq.last s) with
        | (l,_,_) when l >= 3 -> true
        | (2,(f,_),(l,_)) when l-f<>1 -> true
        | _ -> false

    let isIt2 (str:string) =
        let paired = str |> Seq.pairwise
        let overlapping = paired |> Seq.mapi  (fun i e -> (i, e)) |> Seq.groupBy (fun (_, e) -> e) |> Seq.exists verifyMatch
        let hasSandwish = paired |> Seq.pairwise |> Seq.exists (fun ((x,_),(_,y)) -> x=y)
        overlapping && hasSandwish

    let howMany2 (strs:string) =
        strs.Split '\n' |> Array.filter isIt2 |> Array.length