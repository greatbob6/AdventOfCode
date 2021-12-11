<Query Kind="FSharpProgram" />

type Fish = {
    Timer: int
}

let loadFish t = { Timer = t }
let createNewFish () = { Timer = 8 }

let tickFish f =
    if f.Timer = 0 then
        [ createNewFish (); { f with Timer = 6 } ]
    else
        [ { f with Timer = f.Timer - 1 } ]

let simulateOneDay fishes =
    fishes
    |> List.collect tickFish

let simulateFish days fishes =
    [1..days]
    |> List.fold (fun s _ -> simulateOneDay s) fishes
    
let fish = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\6\puzzle_input.txt")
    |> Array.head
    |> (fun s -> s.Split(","))
    |> Array.map (int >> loadFish)
    |> Array.toList

let simmedFish = simulateFish 80 fish |> List.length

simmedFish.Dump()