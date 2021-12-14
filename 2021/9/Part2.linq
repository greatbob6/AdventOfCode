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
    |> (fun b -> if b then (Some (x,y)) else None)
    
let rec getNoneNineNeighbors (heightMap: int[][]) maxX maxY (basin: (int*int) list) (x, y) =
    let validNeighbors =
        ([
            x, y - 1 // up
            x, y + 1 // down
            x - 1, y // left
            x + 1, y // right
        ])
        |> List.choose (fun (nx, ny) -> if (isInBounds maxX maxY (nx, ny)) && (heightMap[ny][nx] < 9) then Some (nx, ny) else None)
        |> List.except basin

    (*
    (x,y).Dump("me")
    basin.Dump("basin")
    validNeighbors.Dump("neighbors")
    *)
    
    let newBasin = if (List.contains (x,y) basin) then basin else (x,y) :: basin

    if List.isEmpty validNeighbors then
        //"no neighbors".Dump()
        newBasin
    else
        validNeighbors
        |> List.fold (fun b n -> getNoneNineNeighbors heightMap maxX maxY b n) newBasin
        //|> List.collect (getNoneNineNeighbors heightMap maxX maxY ((x,y) :: basin))
        //|> List.distinct

let data = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\9\puzzle_input.txt")
    |> Array.map (fun s -> s.ToCharArray() |> Array.map (string >> int))

let maxX = Array.length (Array.head data)
let maxY = Array.length data

let lowPoints =
    Seq.allPairs [0..(maxX - 1)] [0..(maxY - 1)]
    |> Seq.choose (getLowPoint data maxX maxY)

let result =
    lowPoints
    //|> (Seq.tail >> Seq.head >> (getNoneNineNeighbors data maxX maxY []))
    |> Seq.map (getNoneNineNeighbors data maxX maxY [])
    |> Seq.sortByDescending List.length
    |> Seq.take 3
    |> Seq.fold (fun total b -> total * List.length b) 1
    
result.Dump()

