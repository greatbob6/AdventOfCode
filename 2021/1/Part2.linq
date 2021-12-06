<Query Kind="FSharpProgram" />

let data = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\1\Part1_Input.txt")
    |> Seq.map int
    |> Seq.toList

let windows1 = List.windowed 3 data
let windows2 = List.windowed 3 data[1..]

let sums = 
    List.zip (List.truncate (List.length windows2) windows1) windows2
    |> List.map (fun (firstWindow, secondWindow) -> (List.sum firstWindow, List.sum secondWindow))

let numIncreased =
    sums
    |> List.fold (fun state p -> state + (if (fst p) < (snd p) then 1 else 0)) 0
  
numIncreased.Dump()
