module Tests

open Xunit
open FsUnit

open Day01Core.Building
open Day01Inputs

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


[<Theory>]
[<InlineData("(()))", 5)>]
[<InlineData("(()()))())()()(", 7)>]
[<InlineData(")", 1)>]
let ``basement poition`` (input, expected) =
    input |> basementPosition |>  should equal expected


[<Fact>]
let ``big set part 2`` () =
    bigSet |> basementPosition |>  should equal 1771

