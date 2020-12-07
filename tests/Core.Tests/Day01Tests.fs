module Tests

open System
open Xunit
open FsUnit

open Core.Building
open InputsDay01

[<Theory>]
[<InlineData("(())", 0)>]
[<InlineData("()()", 0)>]
[<InlineData("(((", 3)>]
[<InlineData("(()(()(", 3)>]
[<InlineData("))(((((", 3)>]
[<InlineData(")))", -3)>]
[<InlineData(")())())", -3)>]
[<InlineData("())", -1)>]
[<InlineData("))(", -1)>]
let ``small test`` (input, expected) =
    input |> floorCounter |>  should equal expected

[<Fact>]
let ``big set`` () =
    bigSet |> floorCounter |>  should equal 138
