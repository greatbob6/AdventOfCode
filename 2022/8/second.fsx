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
        let indexes =
            seq { y - 1 .. -1 .. 0 }
            |> Seq.takeWhile (fun r -> (grid[r][x]).Height < (grid[y][x]).Height)
        

        (Seq.length indexes) + (if Seq.min indexes > 0 then 1 else 0)

let viewDistanceLeft (grid: Cell array array) x y =
    if x = 0 then
        0
    elif x = 1 then
        1
    else
        let indexes =
            seq { x - 1 .. -1 .. 0 }
            |> Seq.takeWhile (fun c -> (grid[y][c]).Height < (grid[y][x]).Height)
        
        (Seq.length indexes) + (if Seq.min indexes > 0 then 1 else 0)

let viewDistanceRight (grid: Cell array array) x y =
    let lastIndex = getLastIndex grid

    if x = lastIndex then
        0
    elif x = (lastIndex - 1) then
        1
    else
        let indexes =
            seq { x + 1 .. lastIndex }
            |> Seq.takeWhile (fun c -> (grid[y][c]).Height < (grid[y][x]).Height)
        
        (Seq.length indexes) + (if Seq.max indexes < lastIndex then 1 else 0)

let viewDistanceBottom (grid: Cell array array) x y =
    let lastIndex = getLastIndex grid

    if y = lastIndex then
        0
    elif y = (lastIndex - 1) then
        1
    else
        let indexes =
            seq { y + 1 .. lastIndex }
            |> Seq.takeWhile (fun r -> (grid[r][x]).Height < (grid[y][x]).Height)
        
        (Seq.length indexes) + (if Seq.max indexes < lastIndex then 1 else 0)

let cellScore grid x y =
    viewDistanceTop grid x y * viewDistanceLeft grid x y * viewDistanceRight grid x y * viewDistanceBottom grid x y

let grid =
    System.IO.File.ReadAllLines("8/test.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (fun c -> Array.map (fun x -> { Height = int(string(x)) }) c)

let lastIndex = getLastIndex grid


Seq.allPairs (seq { 0 .. lastIndex }) (seq { 0 .. lastIndex })
|> Seq.map (fun (x, y) -> (x, y, (grid[y][x]).Height, cellScore grid x y))
|> Seq.maxBy (fun (_, _, _, s) -> s)


(*
let top = viewDistanceTop grid 2 3
let bottom = viewDistanceBottom grid 2 3
let left = viewDistanceLeft grid 2 3
let right = viewDistanceRight grid 2 3
*)