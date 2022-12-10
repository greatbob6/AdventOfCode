type Cell = {
    Height: int
}

let getLastIndex (grid: Cell array array) =
    Array.length grid[0] - 1

let cellIsOuter (grid: Cell array array) x y =
    let lastIndex = getLastIndex grid

    if x = 0 || x = lastIndex then true
    elif y = 0 || y = lastIndex then true
    else false

let cellIsVisibleFromTop (grid: Cell array array) x y =
    seq { 0 .. y - 1 }
    |> Seq.forall (fun r -> (grid[r][x]).Height < (grid[y][x]).Height)

let cellIsVisibleFromLeft (grid: Cell array array) x y =
    seq { 0 .. x - 1 }
    |> Seq.forall (fun c -> (grid[y][c]).Height < (grid[y][x]).Height)

let cellIsVisibleFromRight (grid: Cell array array) x y =
    let lastIndex = getLastIndex grid

    seq { x + 1 .. lastIndex }
    |> Seq.forall (fun c -> (grid[y][c]).Height < (grid[y][x]).Height)

let cellIsVisibleFromBottom (grid: Cell array array) x y =
    let lastIndex = getLastIndex grid

    seq { y + 1 .. lastIndex }
    |> Seq.forall (fun r -> (grid[r][x]).Height < (grid[y][x]).Height)

let cellIsVisible grid x y =
    if cellIsOuter grid x y then true
    elif cellIsVisibleFromTop grid x y then true
    elif cellIsVisibleFromBottom grid x y then true
    elif cellIsVisibleFromLeft grid x y then true
    elif cellIsVisibleFromRight grid x y then true
    else false

let grid =
    System.IO.File.ReadAllLines("8/puzzle.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (fun c -> Array.map (fun x -> { Height = int(string(x)) }) c)

let lastIndex = getLastIndex grid

Seq.allPairs (seq { 0 .. lastIndex }) (seq { 0 .. lastIndex })
|> Seq.map (fun (x, y) -> cellIsVisible grid x y)
|> Seq.filter id
|> Seq.length