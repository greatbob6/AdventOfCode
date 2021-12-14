<Query Kind="FSharpProgram" />


let isInBounds maxX maxY (x, y) =
    (x >= 0 && x < maxX) && (y >= 0 && y < maxY)
    
let getLowPoint (heightMap: int[][]) maxX maxY (x, y) =
    let xyVal = heightMap[y][x]
    
    ([
        x, y - 1 // up
        x, y + 1 // down
        x - 1, y // left
        x + 1, y // right
    ])
    |> List.choose (fun (nx, ny) -> if isInBounds maxX maxY (nx, ny) then Some (heightMap[ny][nx]) else None)
    |> List.forall (fun v -> xyVal < v)
    |> (fun x -> if x then (Some xyVal) else None)

let data = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\9\puzzle_input.txt")
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))

let maxX = Array.length (Array.head data)
let maxY = Array.length data

let result =
    Seq.allPairs [0..(maxX - 1)] [0..(maxY - 1)]
    |> Seq.choose (getLowPoint data maxX maxY)
    |> Seq.fold (fun total v -> total + v + 1) 0

result.Dump()

