

let findUnique count (str: string) =
    str.ToCharArray()
    |> Array.windowed count
    |> Array.findIndex (fun x -> (Array.length x) = ((Array.distinct >> Array.length) x))
    |> (+) count

let results =
    System.IO.File.ReadAllLines("6/puzzle.txt")
    |> Array.map (findUnique 14)
