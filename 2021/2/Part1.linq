<Query Kind="FSharpProgram" />

type Position = {
    Horizontal: int
    Depth: int
}

let calcPosition (currentPos: Position) (str: string) =
    match str.Split(' ') with
    | [| dir; amount |] ->
        match dir with
        | "forward" -> { currentPos with Horizontal = currentPos.Horizontal + (int amount) }
        | "up" -> { currentPos with Depth = currentPos.Depth - (int amount) }
        | "down" -> { currentPos with Depth = currentPos.Depth + (int amount) }
        | _ -> currentPos
    | _ -> failwith (sprintf "Invalid input string: %s." str)

let position = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\2\puzzle_input.txt")
    |> Seq.fold calcPosition { Horizontal = 0; Depth = 0 }

position.Dump()
(position.Horizontal * position.Depth).Dump()
