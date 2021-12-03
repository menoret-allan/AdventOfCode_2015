module Tests

open System
open Xunit
open FsUnit
open System.Collections.Generic

open Inputs

exception ParsingFailed of string
let toDictionary (map : Map<_, _>) : Dictionary<_, _> = Dictionary(map)

type Target =
    | Intensity of uint16
    | Wire of string

type Op =
    | Val of (uint16)
    | Set of (Target)
    | And of (Target * Target)
    | Or of (Target * Target)
    | Lshift of (Target * int)
    | Rshift of (Target * int)
    | Not of (Target)

type Circuit = Map<string, Op>

let parseWire (wire:string) =
    try 
        wire |> uint16 |> Intensity
    with :? FormatException -> 
        Wire wire

let parseCmd (cmd:string) =
    match (cmd.Split ' ') with
    | [|wire; "->"; target|] -> (target, Set (parseWire wire))
    | [|wire1; "AND"; wire2; "->"; target|] -> (target, And (parseWire wire1, parseWire wire2))
    | [|wire1; "OR"; wire2; "->"; target|] -> (target, Or (parseWire wire1, parseWire wire2))
    | [|wire; "LSHIFT"; shift; "->"; target|] -> (target, Lshift (parseWire wire, int shift))
    | [|wire; "RSHIFT"; shift; "->"; target|] -> (target, Rshift (parseWire wire, int shift))
    | [|"NOT"; wire; "->"; target|] -> (target, Not (parseWire wire))
    | _ -> raise (ParsingFailed $"Failed parsing, unknown command: {cmd}")

let parse (cmds:string) =
    (cmds.Split '\n') |> Array.map parseCmd |> Map.ofArray

let part1 targetWire (circuit: Circuit) =
    let mCircuit = toDictionary circuit
    let set v at =
        mCircuit.[at] <- Val v
        v

    let rec part1Rec t =
        let getIntensity (target: Target) =
            match target with
            | Intensity x -> x
            | Wire w -> set (part1Rec w) w

        let expendOp cmd =
            match cmd with
            | Val (x) -> x
            | Set (x) -> getIntensity x
            | And (x,y) -> (getIntensity x) &&& (getIntensity y)
            | Or (x,y) -> (getIntensity x) ||| (getIntensity y)
            | Lshift (x, shift) -> getIntensity x <<< shift
            | Rshift (x, shift) -> getIntensity x >>> shift
            | Not (x) -> ~~~ (getIntensity x)
        expendOp mCircuit.[t]
    part1Rec targetWire

[<Fact>]
let ``part 1 test 1`` () =
    parse part1Small |> part1 "d" |> should equal 72
[<Fact>]
let ``part 1 test 2`` () =
    parse part1Small |> part1 "g" |> should equal 114
[<Fact>]
let ``part 1 test 3`` () =
    parse part1Small |> part1 "i" |> should equal 65079

[<Fact>]
let ``part 1`` () =
    parse part1big |> part1 "a" |> should equal 3176

[<Fact>]
let ``part 2`` () =
    parse part2big |> part1 "a" |> should equal 14710
