module Day03Tests

open Xunit
open FsUnit

open Day03Core.Movement
open Day03Inputs

[<Theory>]
[<InlineData(">", 2)>]
[<InlineData("^>v<", 4)>]
[<InlineData("^v^v^v^v^v", 2)>]
let ``small test`` (input, expected) =
    input |> countHouses |> should equal expected

[<Fact>]
let ``big test part 01`` () =
    bigSet |> countHouses |> should equal 2592


[<Theory>]
[<InlineData("^v", 3)>]
[<InlineData("^>v<", 3)>]
[<InlineData("^v^v^v^v^v", 11)>]
let ``small test part 2`` (input, expected) =
    input |> countHouses2 |> should equal expected

[<Fact>]
let ``big test part 02`` () =
    bigSet |> countHouses2 |> should equal 2360