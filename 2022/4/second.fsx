open System

let splitElves (str: string) = str.Split(",")
let parseStartAndEnd (str: string) =
    let arr = str.Split("-")
    (int(arr[0]), int(arr[1]))

let hasOverlap (start1, end1) (start2, end2) =
    (start1 >= start2 && start1 <= end2) || (end1 >= start2 && end1 <= end2) || (start2 >= start1 && start2 <= end1) || (end2 >= start1 && end2 <= end1)

let results =
    IO.File.ReadAllLines("4/puzzle.txt")
    |> Array.map splitElves
    |> Array.map (fun elves -> parseStartAndEnd elves[0], parseStartAndEnd elves[1])
    |> Array.map (fun x -> x ||> hasOverlap)
    |> Array.map (fun x -> if x then 1 else 0)
    |> Array.sum
