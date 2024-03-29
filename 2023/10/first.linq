<Query Kind="FSharpProgram" />

let dumper x = Util.FixedFont(x).Dump()
let GetInputFilePath filename = Path.Join(Path.GetDirectoryName(Util.CurrentQueryPath), filename)

let lines = File.ReadAllLines(GetInputFilePath "input_puzzle.txt")

type Point = {
    X: int
    Y: int
}

let findS (a: char array array) =
    a
    |> Array.mapi (fun i ca -> (ca |> Array.tryFindIndex (fun c -> c = 'S'), i))
    |> Array.map (fun (xo, y) -> match xo with | None -> None | Some x -> Some { X = x; Y = y })
    |> Array.choose id
    |> Array.head

let grid =
    lines
    |> Array.map (fun s -> s.ToCharArray())
    
let start = 
    grid
    |> findS
    
// | is a vertical pipe connecting north and south.
// - is a horizontal pipe connecting east and west.
// L is a 90-degree bend connecting north and east.
// J is a 90-degree bend connecting north and west.
// 7 is a 90-degree bend connecting south and west.
// F is a 90-degree bend connecting south and east.

let findNextPoint prev cur =
    //(grid[cur.Y][cur.X]).Dump()
    match grid[cur.Y][cur.X] with
    | '|' -> { X = cur.X; Y = cur.Y + (cur.Y - prev.Y) }
    | '-' -> { X = cur.X + (cur.X - prev.X); Y = cur.Y }
    | 'L' -> { X = cur.X + (cur.Y - prev.Y); Y = cur.Y + (cur.X - prev.X) }
    | 'J' -> { X = cur.X - (cur.Y - prev.Y); Y = cur.Y - (cur.X - prev.X) }
    | '7' -> { X = cur.X + (cur.Y - prev.Y); Y = cur.Y + (cur.X - prev.X) }
    | 'F' -> { X = cur.X - (cur.Y - prev.Y); Y = cur.Y - (cur.X - prev.X) }
    | _ -> { X = -1; Y = -1 }

type CycleResult =
| NoCycle of Point list
| Cycle of Point list

let rec findCycle prevPoint curPoint previousVisits =
    let nextPoint = findNextPoint prevPoint curPoint
    
    let latestVisits = curPoint :: previousVisits
    
    match nextPoint with
    | { X = -1; Y = -1 } -> NoCycle (List.rev latestVisits)
    | _ when nextPoint.X < 0 || nextPoint.X >= (Array.length grid[0]) || nextPoint.Y < 0 || nextPoint.Y >= (Array.length grid) -> NoCycle (List.rev latestVisits)
    | _ when nextPoint = start -> Cycle (List.rev latestVisits)
    | _ when List.contains nextPoint previousVisits -> NoCycle (List.rev latestVisits)
    | _ -> findCycle curPoint nextPoint latestVisits

let getFurthestDistance c =
    let steps = match c with | Cycle s -> s
    
    steps |> List.length |> (fun x -> (float x) / 2.) |> ceil |> int

[| { X = start.X; Y = start.Y - 1 }; { X = start.X + 1; Y = start.Y }; { X = start.X; Y = start.Y + 1 }; { X = start.X - 1; Y = start.Y } |]
|> Array.filter (fun p -> p.X >= 0 && p.X < (Array.length grid[0]) && p.Y >= 0 && p.Y < (Array.length grid) )
|> Array.Parallel.map (fun p -> findCycle start p [])
|> Array.filter (function | Cycle _ -> true | _ -> false)
|> Array.map getFurthestDistance
|> Array.head
|> dumper
