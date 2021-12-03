module Tests

open System
open Xunit
open FsUnit

open Inputs

let stringCodeCalc (strs: string array) =
    strs |> Array.sumBy (fun x -> x.Length)

let rec countMem (str: char list) =
    match str with
    | [] -> 0
    | '\\' :: '\\' :: rest -> 1 + countMem rest
    | '\\' :: '"' :: rest -> 1 + countMem rest
    | '\\' :: 'x' :: _ :: _ :: rest -> 1 + countMem rest
    | x::rest -> 1 + countMem rest

let rec countGen (str: char list) =
    match str with
    | [] -> 2
    | '"' :: rest -> 2 + countGen rest
    | '\\' :: rest -> 2 + countGen rest
    | x::rest -> 1 + countGen rest

let stringMemCalc (strs: string array) =
    strs |> Array.sumBy (fun x -> x |> Seq.skip 1 |> Seq.take (x.Length - 2) |> Seq.toList |> countMem)

let stringGenCalc (strs: string array) =
    strs |> Array.sumBy (fun x -> x |> Seq.toList |> countGen)

let parse (str:string) = str.Split '\n' |> Array.filter ((<>) "")

[<Fact>]
let ``stringCodeCalc tests`` () =
    parse small |> stringCodeCalc |> should equal 23

[<Fact>]
let ``stringMemCalc tests`` () =
    parse small |> stringMemCalc |> should equal 11

[<Fact>]
let ``big stringCodeCalc tests`` () =
    parse big |> stringCodeCalc |> should equal 6202

[<Fact>]
let ``big stringMemCalc tests`` () =
    parse big |> stringMemCalc |> should equal 4860

[<Fact>]
let ``stringGenCalc tests 2`` () =
    parse small |> stringGenCalc |> should equal 42

[<Fact>]
let ``big stringGenCalc tests 2`` () =
    parse big |> stringGenCalc |> should equal 8276
