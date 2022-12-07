

let findUniqueFour (str: string) =
    str.ToCharArray()
    |> Array.windowed 4
    |> Array.findIndex (fun x -> (Array.length x) = ((Array.distinct >> Array.length) x))
    |> (+) 4

let results =
    System.IO.File.ReadAllLines("6/puzzle.txt")
    |> Array.map findUniqueFour
