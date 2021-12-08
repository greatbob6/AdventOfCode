<Query Kind="FSharpProgram" />

type Position = {
    Horizontal: int
    Depth: int
    Aim: int
}

let calcPosition (currentPos: Position) (str: string) =
    let (dir, amount) = match str.Split(' ') with
                        | [| str1; str2 |] -> (str1, (int str2))
                        | _ -> failwith (sprintf "Invalid input string: %s." str)
        
    match dir with
    | "forward" -> { currentPos with Horizontal = currentPos.Horizontal + amount; Depth = currentPos.Depth + (currentPos.Aim * amount) }
    | "up" -> { currentPos with Aim = currentPos.Aim - amount }
    | "down" -> { currentPos with Aim = currentPos.Aim + amount }
    | _ -> currentPos

let position = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\2\puzzle_input.txt")
    |> Seq.fold calcPosition { Horizontal = 0; Depth = 0; Aim = 0 }

position.Dump()
(position.Horizontal * position.Depth).Dump()
