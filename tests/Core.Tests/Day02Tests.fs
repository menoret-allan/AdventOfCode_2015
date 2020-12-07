module Day02Tests

open Xunit
open FsUnit

open Day02Core.Warpping
open Day02Parser.Warpping
open Day02Inputs

[<Theory>]
[<InlineData(2,3,4, 58)>]
[<InlineData(1,1,10, 43)>]
let ``small test`` (x,y,z, expected) =
    (x,y,z) |> computePackage |>  should equal expected

[<Fact>]
let ``big set part 01`` () =
    bigSet |> getDimension |> computePackages |> should equal 1586300

[<Theory>]
[<InlineData(2,3,4, 34)>]
[<InlineData(1,1,10, 14)>]
[<InlineData(42,69,10, 29084)>]
let ``small test ribbon`` (x,y,z, expected) =
    (x,y,z) |> computeRibbon |>  should equal expected

[<Fact>]
let ``big set part 02 ribbon`` () =
    bigSet |> getDimension |> computeRibbons |> should equal 3737498
