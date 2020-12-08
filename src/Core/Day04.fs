namespace Day04Core



open System.Security.Cryptography
open System.Text

    
module Bitcoins =

    let (|Prefix|_|) (p:string) (s:string) =
        if s.StartsWith(p) then
            Some(s.Substring(p.Length))
        else
            None

    let MD5Hash (input : string) =
       use md5 = System.Security.Cryptography.MD5.Create()
       input
       |> System.Text.Encoding.ASCII.GetBytes
       |> md5.ComputeHash
       |> Seq.map (fun c -> c.ToString("X2"))
       |> Seq.reduce (+)


    let rec infinteNumber n =
        seq {yield n; yield! infinteNumber (n+1)}

    let nbMatchMD5 str (n: int) =
        let md5 = MD5Hash (str + string n)
        match md5 with
        | Prefix "00000" rest -> true
        | _ -> false

    let nbMatchMD5Part2 str (n: int) =
        let md5 = MD5Hash (str + string n)
        match md5 with
        | Prefix "000000" rest -> true
        | _ -> false

    let hack (str:string) =
        infinteNumber 1 |> Seq.find (nbMatchMD5 str) 

    let hack2 (str:string) =
        infinteNumber 1 |> Seq.find (nbMatchMD5Part2 str) 