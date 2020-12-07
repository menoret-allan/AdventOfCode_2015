namespace Day02Core

module Warpping =
    let computePackage (x, y, z) =
        let all = [x*y;x*z;y*z]
        all |> List.sumBy ((*)2) |> (+) (all |> List.min)
    let computePackages arr =
        arr |> Array.sumBy computePackage

    let computeRibbon (x, y, z) =
        let all = [x;y;z] |> List.sort
        all |> List.take 2 |> List.sumBy ((*)2) |> (+) (all |> List.reduce (*))
    let computeRibbons arr =
        arr |> Array.sumBy computeRibbon