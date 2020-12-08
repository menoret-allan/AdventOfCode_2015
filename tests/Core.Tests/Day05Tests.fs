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
