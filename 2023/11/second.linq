<Query Kind="FSharpProgram" />

let dumper x = Util.FixedFont(x).Dump()
let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

let findGalaxies g =
    g
    |> Array.mapi (fun y row -> row |> Array.mapi (fun x col -> ((x |> int64, y |> int64), col = '#')))
    |> Array.collect id
    |> Array.filter snd
    |> Array.map fst

let expandGalaxies (gs: (int64 * int64) array) (expandRows: int64 array) (expandCols: int64 array) (expandBy: int64) =
    let calcExpandedX (x: int64) cols expandNum =
        x + (cols |> Array.filter (fun v -> v < x) |> Array.length |> int64) * expandNum
        
    let calcExpandedY (y: int64) rows expandNum =
        y + (rows |> Array.filter (fun v -> v < y) |> Array.length |> int64) * expandNum
    
    gs
    |> Array.map (fun (x, y) -> (calcExpandedX x expandCols expandBy), (calcExpandedY y expandRows expandBy))
    
let grid =
    lines
    |> Array.map (fun s -> s.ToCharArray())
    
let expandRows =
    grid
    |> Array.mapi (fun i r -> (i |> int64, r |> Array.forall (fun x -> x = '.')))
    |> Array.filter snd
    |> Array.map fst
    
let expandCols =
    seq { 0L .. (Array.length grid[0] |> int64) - 1L }
    |> Seq.map (fun x -> (x, seq { 0L .. (Array.length grid |> int64) - 1L } |> Seq.forall (fun y -> grid[y |> int][x |> int] = '.')))
    |> Seq.filter snd
    |> Seq.map fst
    |> Seq.toArray

let galaxies = findGalaxies grid
let expandedGalaxies = expandGalaxies galaxies expandRows expandCols (1000000L - 1L)

let galaxyPairs =
    Array.allPairs expandedGalaxies expandedGalaxies
    |> Array.filter (fun (a, b) -> a <> b)
    |> Array.map (fun (a, b) -> if a < b then (a, b) else (b, a))
    |> Array.distinct

let findMinDistance (x1,y1) (x2,y2) =
    abs (x2 - x1) + abs (y2 - y1)

galaxyPairs
|> Array.map (fun (p1, p2) -> (p1, p2, findMinDistance p1 p2))
|> Array.sumBy (fun (_, _, d) -> d)
|> dumper
