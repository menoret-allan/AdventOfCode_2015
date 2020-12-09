module Day06Tests

open Xunit
open FsUnit

open Day06Inputs
open Day06Core.FireHazard
open Day06Parser.FireHazardParser

[<Fact>]
let ``small set part 01`` () =
    smallSet |> scan |> howManyOn |> should equal 998996

// turn off because super slow copy the map all the time <3
// [<Fact>]
// let ``big set part 01`` () =
//     bigSet |> scan |> howManyOn |> should equal 569999


