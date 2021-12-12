<Query Kind="FSharpProgram" />


let inline median input = 
    let sorted = input |> Array.sort
    let m1,m2 = 
        let len = sorted.Length - 1 |> float
        len / 2. |> floor |> int, len / 2. |> ceil |> int 
    
    (sorted[m1] + sorted[m2] |> float) / 2.
    
let sumN n = (n * (n + 1)) / 2
    
let crabs = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\7\puzzle_input.txt")
    |> Array.head
    |> (fun s -> s.Split(","))
    |> Array.map int

let meanValue =
    crabs
    |> Array.map double
    |> Array.average
//    |> round

meanValue.Dump()

(meanValue |> round).Dump()

(int meanValue).Dump()

let fuel =
    crabs
    |> Array.map (fun c -> (abs (c - (int meanValue))) |> sumN)
    |> Array.sum

fuel.Dump()

// 92676748
// 92676646