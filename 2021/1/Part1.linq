<Query Kind="FSharpProgram" />

let data = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\1\Part1_Input.txt")
    |> Seq.map int
    |> Seq.toList

let numIncreased =
    data
    |> List.pairwise
    |> List.fold (fun state p -> state + (if (fst p) < (snd p) then 1 else 0)) 0
    
numIncreased.Dump()
