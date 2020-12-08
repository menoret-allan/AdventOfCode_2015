module Day04Tests

open Xunit
open FsUnit

open Day04Core.Bitcoins

[<Theory>]
[<InlineData("abcdef", 609043)>]
[<InlineData("pqrstuv", 1048970)>]
let ``small test part 2`` (input, expected) =
    input |> hack |> should equal expected

[<Fact>]
let ``big test part 01`` () =
    "ckczppom" |> hack |> should equal 117946

[<Fact>]
let ``big test part 02`` () =
    "ckczppom" |> hack2 |> should equal 3938038