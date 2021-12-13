<Query Kind="FSharpProgram" />

type Test = {
    Inputs: char[][]
    OutputDigits: char[][]
}

let parseInsAndOuts [| (is: string); (os: string) |] =
    let inputs =
        is.Split(" ")
        |> Array.map (fun x -> x.ToCharArray())
        
    let outputs =
        os.Split(" ")
        |> Array.map (fun x -> x.ToCharArray())
    
    { Inputs = inputs; OutputDigits = outputs }

let foldDigits count (chars: char[]) =
    count + match (Array.length chars) with
            | 2 | 4 | 3 | 7 -> 1
            | _ -> 0
    
let data = 
    System.IO.File.ReadAllLines(@"C:\Users\great\OneDrive\Documents\Programming\AdventOfCode\2021\8\puzzle_input.txt")
    |> Array.map (fun s -> s.Split(" | "))
    |> Array.map parseInsAndOuts

let result =
    data
    |> Array.collect (fun x -> x.OutputDigits)
    |> Array.fold foldDigits 0
    
result.Dump()
