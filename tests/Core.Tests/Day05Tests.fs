module Day05Tests

open Xunit
open FsUnit

open Day05Core.Nice
open Day05Inputs

[<Theory>]
[<InlineData("ugknbfddgicrmopn", true)>]
[<InlineData("aaa", true)>]
[<InlineData("jchzalrnumimnmhp", false)>]
[<InlineData("haegwjzuvuyypxyu", false)>]
[<InlineData("dvszwmarrgswjxmb", false)>]
let ``small test part 1`` (input, expected) =
    input |> isIt |> should equal expected

[<Fact>]
let ``big test part 01`` () =
    bigSet |> howMany |> should equal 255

[<Theory>]
[<InlineData("qjhvhtzxzqqjkmpb", true)>]
[<InlineData("xxyxx", true)>]
[<InlineData("aaa", false)>]
[<InlineData("uurcxstgmygtbstg", false)>]
[<InlineData("ieodomkazucvgmuy", false)>]
let ``small test part 2`` (input, expected) =
    input |> isIt2 |> should equal expected

[<Fact>]
let ``big test part 02`` () =
    bigSet |> howMany2 |> should equal 55
