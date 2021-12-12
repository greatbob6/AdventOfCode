<Query Kind="FSharpProgram" />


let inline median input = 
    let sorted = input |> Array.sort
    let m1,m2 = 
        let len = sorted.Length - 1 |> float
        len / 2. |> floor |> int, len / 2. |> ceil |> int 
    
    (sorted[m1] + sorted[m2] |> float) / 2.
    
let crabs = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\7\puzzle_input.txt")
    |> Array.head
    |> (fun s -> s.Split(","))
    |> Array.map int

let medianValue = crabs |> median |> int

let fuel =
    crabs
    |> Array.map (fun c -> abs (c - medianValue))
    |> Array.sum

fuel.Dump()