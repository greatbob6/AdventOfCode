<Query Kind="FSharpProgram" />

type Element = {
    Value: int
    Marked: bool
}

type Board = {
    Elements: Element[][]
}

type PuzzleData = {
    Draws: int[]
    Boards: Board[]
}

let parseDraws (draws: string) =
    draws.Split(",")
    |> Array.map ((fun s -> s.Trim()) >> int)

let parseBoardLine (line: string) =
    [|
        { Value = (int line[0..1]); Marked = false }
        { Value = (int line[3..4]); Marked = false }
        { Value = (int line[6..7]); Marked = false }
        { Value = (int line[9..10]); Marked = false }
        { Value = (int line[12..13]); Marked = false }
    |]

let rec parseBoards (inputLines: string list) =
    let boardLines, rest = inputLines |> List.splitAt 6
    
    let board = {
        Elements =
            boardLines
            |> List.skip 1
            |> List.map parseBoardLine
            |> List.toArray
    }

    match rest with
    | [] -> [ board ]
    | _ -> board :: (parseBoards rest)

let readInputData inputLines =
    match inputLines with
    | draws :: rest -> { Draws = (parseDraws draws); Boards = (parseBoards rest) |> List.toArray }
    | _ -> failwith "BROKEN!"

let updateElement draw element =
    if element.Value = draw then { element with Marked = true } else element

let updateBoard draw (board: Board) =
    {
        Elements = board.Elements
            |> Array.map (fun row -> row |> Array.map (updateElement draw))
    }
    
let boardWon board =
    Array.concat [ board.Elements; (Array.transpose board.Elements) ]
    |> Array.exists (fun row -> (Array.forall (fun e -> e.Marked) row))

let rec drawLoop boardsRemaining drawsRemaining winningBoards =
    let draw = Array.head drawsRemaining
    let updatedBoards = Array.map (updateBoard draw) boardsRemaining

    let (newWins, notWon) =
        updatedBoards
        |> Array.partition boardWon
        
    let totalWins = 
        newWins
        |> Array.fold (fun winList board -> (board, draw) :: winList) winningBoards

    match notWon with
    | [| |] -> totalWins
    | _ -> drawLoop notWon (Array.tail drawsRemaining) totalWins
    
let calculateBoardScore (board, draw) =
    board.Elements
    |> Array.fold (fun sum row -> sum + Array.sum (Array.map (fun e -> if e.Marked then 0 else e.Value) row)) 0
    |> (*) draw
    
let inputLines = 
    System.IO.File.ReadAllLines(@"C:\Users\lv5640\Programming\AdventOfCode\2021\4\puzzle_input.txt")
    |> Array.toList

let puzzleData = readInputData inputLines

let result =
    drawLoop puzzleData.Boards puzzleData.Draws []
    |> List.head
    |> calculateBoardScore
                
result.Dump()

