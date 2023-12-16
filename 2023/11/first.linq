<Query Kind="FSharpProgram" />

let dumper x = Util.FixedFont(x).Dump()
let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

type Point = {
    X: int
    Y: int
}

let gridViewer (g: char array array) =
    g
    |> Array.map (fun c -> String c)
    |> dumper
    
let grid =
    lines
    |> Array.map (fun s -> s.ToCharArray())
    
let expandRows =
    grid
    |> Array.mapi (fun i r -> (i, r |> Array.forall (fun x -> x = '.')))
    |> Array.filter snd
    |> Array.map fst
    
let expandCols =
    seq { 0 .. (Array.length grid[0]) - 1 }
    |> Seq.map (fun x -> (x, seq { 0 .. (Array.length grid) - 1 } |> Seq.forall (fun y -> grid[y][x] = '.')))
    |> Seq.filter snd
    |> Seq.map fst
    |> Seq.toArray
    
let gridWithMoreRows =
    expandRows
    |> Array.rev
    |> Array.fold (fun g rowIdx -> g |> Array.insertAt rowIdx (Array.create (Array.length grid[0]) '.')) grid
    
let expandedGrid =
    gridWithMoreRows
    |> Array.map (fun row -> expandCols |> Array.rev |> Array.fold (fun r colIdx -> r |> Array.insertAt colIdx '.') row)

let galaxies =
    expandedGrid
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x col -> ((x, y), col = '#')))
    |> Array.collect id
    |> Array.filter snd
    |> Array.map fst

let galaxyPairs =
    Array.allPairs galaxies galaxies
    |> Array.filter (fun (a, b) -> a <> b)
    |> Array.map (fun (a, b) -> if a < b then (a, b) else (b, a))
    |> Array.distinct

let findMinDistance (x1,y1) (x2,y2) =
    abs (x2 - x1) + abs (y2 - y1)
    
galaxyPairs
|> Array.map (fun (p1, p2) -> findMinDistance p1 p2)
|> Array.sum
|> dumper
    
