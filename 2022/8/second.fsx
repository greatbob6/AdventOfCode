type Cell = {
    Height: int
}

let getLastIndex (grid: Cell array array) =
    Array.length grid[0] - 1

let viewDistanceTop (grid: Cell array array) x y =
    if y = 0 then
        0
    elif y = 1 then
        1
    else
        let idx =
            seq { y - 1 .. -1 .. 0 }
            |> Seq.fold (fun s r -> if s > -1 then s elif (grid[r][x]).Height >= (grid[y][x]).Height || r = 0 then r else s) -1
            
        y - idx
        
let viewDistanceLeft (grid: Cell array array) x y =
    if x = 0 then
        0
    elif x = 1 then
        1
    else
        let idx =
            seq { x - 1 .. -1 .. 0 }
            |> Seq.fold (fun s c -> if s > -1 then s elif (grid[y][c]).Height >= (grid[y][x]).Height || c = 0 then c else s) -1
            
        x - idx

let viewDistanceRight (grid: Cell array array) x y =
    let lastIndex = getLastIndex grid

    if x = lastIndex then
        0
    elif x = (lastIndex - 1) then
        1
    else
        let idx =
            seq { x + 1 .. lastIndex }
            |> Seq.fold (fun s c -> if s > -1 then s elif (grid[y][c]).Height >= (grid[y][x]).Height || c = lastIndex then c else s) -1
            
        idx - x

let viewDistanceBottom (grid: Cell array array) x y =
    let lastIndex = getLastIndex grid

    if y = lastIndex then
        0
    elif y = (lastIndex - 1) then
        1
    else
        let idx =
            seq { y + 1 .. lastIndex }
            |> Seq.fold (fun s r -> if s > -1 then s elif (grid[r][x]).Height >= (grid[y][x]).Height || r = lastIndex then r else s) -1
        
        idx - y

let cellScore grid x y =
    viewDistanceTop grid x y * viewDistanceLeft grid x y * viewDistanceRight grid x y * viewDistanceBottom grid x y

let grid =
    System.IO.File.ReadAllLines(@"8\puzzle.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (fun c -> Array.map (fun x -> { Height = int(string(x)) }) c)

let lastIndex = getLastIndex grid


let result =
    Seq.allPairs (seq { 0 .. lastIndex }) (seq { 0 .. lastIndex })
    |> Seq.map (fun (x, y) -> (x, y, (grid[y][x]).Height, cellScore grid x y))
    |> Seq.maxBy (fun (_, _, _, s) -> s)

(*
let top = viewDistanceTop grid 2 3
let bottom = viewDistanceBottom grid 2 3
let left = viewDistanceLeft grid 2 3
let right = viewDistanceRight grid 2 3
*)