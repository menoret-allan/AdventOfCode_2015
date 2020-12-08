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